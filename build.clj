(ns build
  (:require [clojure.tools.build.api :as b]))

(defn compile-java
      "Compiles Java sources (DelayState)."
      [_]
      (let [basis (b/create-basis)]
           (b/javac {:src-dirs ["java"]
                     :class-dir "target/classes"
                     :basis basis})))
