(ns net.lewissship.delayed-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [net.lewisship.reset :as r]
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

    (r/reset-state! d)

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

    (r/reset-state! d)

    (is (= [[:destructed ::value]] @*log))))


(d/defdelay with-docstring
  "Docstring for with-docstring"
  (do
    (log :with-docstring)
    (+ 1 2)))


(deftest docstring-conveyed-to-meta
  (is (= "Docstring for with-docstring"
         (-> #'with-docstring meta :doc)))

  (d/reset-saved!)

  (is (= 3 @with-docstring))

  (is (= [:with-docstring] @*log)))

(d/defdelay all-options
  "All options"
  (do
    (log :construct-all-options)
    42)
  (fn [v]
    (log [:destruct-all-options v])))

(deftest defdelay-all-options
  (is (= [] @*log))

  (is (= false (realized? all-options)))

  (is (= 42 @all-options))

  (is (= [:construct-all-options] @*log))

  (wipe-log)

  (r/reset-state! all-options)

  (is (= false (realized? all-options)))

  (is (= [[:destruct-all-options 42]]) @*log))
