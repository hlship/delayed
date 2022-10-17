(ns net.lewisship.reset)

(defprotocol IReset
  "Allows a stateful object to be reset to base state."

  (reset-state! [this]
    "Resets the state of this object, returning the object in its reset state.

    Should return the object."))
