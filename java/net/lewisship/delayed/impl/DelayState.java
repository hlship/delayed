package net.lewisship.delayed.impl;

// This is mutable state inside a ResettableDelay,
// used as part of a workaround for https://clojure.atlassian.net/browse/CLJ-1708

public class DelayState {
    public volatile Object value;
    public volatile Throwable exception;
    public volatile boolean isRealized;
}
