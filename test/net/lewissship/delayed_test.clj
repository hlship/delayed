(ns net.lewissship.delayed-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [net.lewisship.delayed :as d]))

(def *log (atom []))

(defn- wipe-log [] (swap! *log empty))

(defn- log [v] (swap! *log conj v))

(use-fixtures :each
              (fn [f]
                (try
                  (f)
                  (finally
                    (wipe-log)))))

(deftest just-constructor
  (let [*value (atom 10000)
        d  (d/delayed (do
                        (log [:constructed @*value])
                        (swap! *value inc)))]
    (is (= false (realized? d)))

    (is (= 10001 @d))

    (is (= [[:constructed 10000]] @*log))

    (is (realized? d))

    (wipe-log)

    @d

    (is (= [] @*log))

    (d/reset-state! d)

    (is (= false (realized? d)))

    (is (= 10002 @d))

    (is (= [[:constructed 10001]] @*log))))

(deftest with-destructor
  (let [d (d/delayed (do
                       (log :constructed)
                       ::value)
                     (fn [v]
                       (log [:destructed v])))]
    (is (= ::value @d))

    (is (= [:constructed] @*log) )

    (wipe-log)

    (d/reset-state! d)

    (is (= [[:destructed ::value]] @*log))))
