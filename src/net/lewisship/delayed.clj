(ns net.lewisship.delayed
  (:import (clojure.lang IDeref IPending IFn)))

(defprotocol IReset
  "Allows a stateful object to be reset to base state."

  (reset-state! [this]
    "Resets the state of this object, returning the object in its reset state."))

;; A re-implementation of clojure.lang.Delay that allows for the delay to reset
;; back to unrealized.
(deftype ResettableDelay [^:volatile-mutable value
                         ^:volatile-mutable ^Throwable exception
                         ^:volatile-mutable realized?
                         ^IFn constructor
                          ^IFn destructor]


  IDeref
  (deref [this]
    (when-not realized?
      (locking this
        (when-not realized?
          (try
            (set! value (constructor))
            (catch Throwable e
                   (set! exception e)))
          (set! realized? true))))

    (if exception
      (throw exception)
      value))

  IPending
  (isRealized [_]
    realized?)

  IReset
  (reset-state! [this]
    (locking this
      ;; If realized and non-null then let the destructor function destroy the value;
      ;; this is intended for things like closing database connections or shutting
      ;; down thread pools.
      (when (and realized?
                 value
                 destructor)
        (destructor value))
      (set! realized? false)
      (set! value nil)
      (set! exception nil))

    this))

(defn new-resettable-delay
  "Creates a new resettable delay object around the no-args constructor function,
  which provides the value on first de-reference."
  ([constructor]
   (new-resettable-delay constructor nil))
  ([constructor destructor]
   (ResettableDelay. nil nil false constructor destructor)))

;; TODO: Use some kind of weak reference instead, to allow for new defs during
;; REPL development.
(defonce ^:private *delays (atom []))

(defn reset-all!
  "Resets all previously defined delay objects."
  []
  (locking *delays
    (run! reset-state! @*delays)))

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
  "Defines a var for a resettable delay."
  ([sym init]
   `(defdelay sym nil init))
  ([sym docstring init]
   {:pre [(or (nil? docstring) (string? docstring))
          (some? init)]}
   (let [sym-meta (cond-> (meta sym)
                    docstring (assoc :doc docstring))
         {:keys [destructor]} sym-meta
         constructor `(fn [] ~init)]
     `(def ~(with-meta sym sym-meta)
        (save! (new-resettable-delay ~constructor ~destructor))))))
