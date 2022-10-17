(ns net.lewisship.delayed
  (:require [net.lewisship.reset :as r])
  (:import (clojure.lang IDeref IPending)
           (net.lewisship.delayed.impl DelayState)
           (java.lang.ref ReferenceQueue Reference WeakReference)))

;; A re-implementation of clojure.lang.Delay that allows for the delay to reset
;; back to unrealized.
(deftype ResettableDelay [^DelayState state]
  IDeref
  (deref [_]
    (.realizeValue state))

  IPending
  (isRealized [_]
    (.isRealized state))

  r/IReset
  (reset-state! [this]
    (.reset state)

    this))

(defn new-resettable-delay
  "Creates a new resettable delay object around the no-args constructor function,
  which provides the value on first de-reference."
  ([constructor]
   (new-resettable-delay constructor nil))
  ([constructor destructor]
   (ResettableDelay. (DelayState. constructor destructor))))

(defn reset-saved!
  "Resets all previously saved delay objects."
  []
  ;; Turn References back into ResettableDelays (unless the reference has been cleared)
  (run! r/reset-state! (keep #(.get ^Reference %) @*delays)))

(defn save!
  "Saves a delay so that it can later be reset by [[reset-all!]].

  The k is typically a qualified symbol for the delay.  When a delay is saved for the key,
  and prior delay for the key is reset.

  Returns the delay."
  [k ^ResettableDelay delay]
  {:pre [(some? delay)]}
  (loop []
    (let [delays @*delays
          prior (get delays k)
          delays' (assoc delays k delay)]
      (if (compare-and-set! *delaysπ))
      )
    (swap! *delays conj r))
  delay)

(defmacro delayed
  "Similar clojure.core/delay but returns a ResettableDelay.

  A single form is the constructor, an expression that will be invoked
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

  The delay is passed to [[save!]] to it can be reset by [[reset-all!]].

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
