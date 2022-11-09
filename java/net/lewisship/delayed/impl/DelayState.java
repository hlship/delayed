package net.lewisship.delayed.impl;

// This is mutable state inside a ResettableDelay,
// used as part of a workaround for https://clojure.atlassian.net/browse/CLJ-1708

import clojure.lang.IFn;

public class DelayState {
    public volatile Object value;
    public volatile Throwable exception;
    public volatile boolean isRealized;
    private final IFn constructor;
    private final IFn destructor;
    public DelayState(IFn constructor, IFn destructor) {
        this.constructor = constructor;
        this.destructor = destructor;
    }
    public Object realizeValue() throws Throwable {
        if (!isRealized) {
            synchronized (this) {
                if (!isRealized) {
                    try {
                        value = constructor.invoke();
                    } catch (Throwable t) {
                        exception = t;
                    }

                    isRealized = true;
                }
            }
        }

        if (exception != null) {
            throw exception;
        }

        return value;
    }
    public synchronized void reset() {
        if (isRealized && destructor != null && value != null) {
            destructor.invoke(value);
        }

        value = null;
        exception = null;
        isRealized = false;
    }
}
