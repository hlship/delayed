(ns net.lewisship.delayed
  (:import (clojure.lang IDeref IPending IFn)
           (net.lewisship.delayed.impl DelayState)))

(defprotocol IReset
  "Allows a stateful object to be reset to base state."

  (reset-state! [this]
    "Resets the state of this object, returning the object in its reset state."))

;; A re-implementation of clojure.lang.Delay that allows for the delay to reset
;; back to unrealized.
(deftype ResettableDelay [^DelayState state
                          ^IFn constructor
                          ^IFn destructor]
  IDeref
  (deref [this]
    (when-not (.isRealized state)
      (locking this
        (when-not (.isRealized state)
          (try
            (set! (. state value) (constructor))
            (catch Throwable e
              (set! (. state exception) e)))
          (set! (. state isRealized) true))))

    (if-let [exception (.exception state)]
      (throw exception)
      (.value state)))

  IPending
  (isRealized [_]
    (.isRealized state))

  IReset
  (reset-state! [this]
    (locking this
      ;; If realized and non-null then let the destructor function destroy the value;
      ;; this is intended for things like closing database connections or shutting
      ;; down thread pools.
      (when (and destructor (.isRealized state))
        (when-let [value (.value state)]
          (destructor value)))
      (set! (. state isRealized) false)
      (set! (. state value) nil)
      (set! (. state exception) nil))

    this))

(defn new-resettable-delay
  "Creates a new resettable delay object around the no-args constructor function,
  which provides the value on first de-reference."
  ([constructor]
   (new-resettable-delay constructor nil))
  ([constructor destructor]
   (ResettableDelay. (DelayState.) constructor destructor)))

;; TODO: Use some kind of weak reference instead, to allow for new defs during
;; REPL development.
(defonce ^:private *delays (atom []))

(defn reset-all!
  "Resets all previously defined delay objects."
  []
  (locking *delays
    (run! reset-state! @*delays)))

;; TODO: May make sense for *delays to be a map keyed on Var, or the weak hash map idea.
(defn save!
  "Saves a delay so that it can later be reset.  Returns the delay."
  [^ResettableDelay delay]
  {:pre [(some? delay)]}
  (swap! *delays conj delay)
  delay)

(defmacro delayed
  "Similar clojure.core/delay but returns a ResettableDelay.

  A single form is the constructor, and expression that will be invoked
  once, on demand, to supply the delayed value (it will be invoked again
  after the delay is reset).

  With two forms, the first is the constructor expression, the second is the destructor
  function; the destructor is passed the constructed value when the containing
  ResettableDelay is reset. The return value of the destructor is ignored."
  ([constructor]
   `(new-resettable-delay (fn [] ~constructor)))
  ([constructor destructor]
   `(new-resettable-delay (fn [] ~constructor) ~destructor)))

(defmacro defdelay
  "Defines a var for a resettable delay.

  The init expression is required; it may be prefixed by
  a docstring, and suffixed by a destructor function."
  {:arglists '([docstring? init-expression destructor-fn?])}
  [sym & terms]
  (let [maybe-docstring (first terms)
        [docstring terms] (if (string? maybe-docstring)
                            [maybe-docstring (rest terms)]
                            [nil terms])
        _ (assert (seq terms)
                  "Missing init expression")
        [init destructor & more] terms
        _ (assert (nil? more)
                  "Extra arguments beyond docstring, init expression, and destructor function")
        sym-meta (cond-> (meta sym)
                   docstring (assoc :doc docstring))
        constructor `(fn [] ~init)]
    `(def ~(with-meta sym sym-meta)
       (save! (new-resettable-delay ~constructor ~destructor)))))
