(ns mwdict.core-test
  (:use clojure.test
        mwdict.core
        [clojure.java.io :only [resource]]
        [mwdict.text :only [render]]))

(defn- validate-word [w]
  (is (= (render (search :collegiate :xml w))
         (slurp (resource (str "console-output/" w)))) w))

(def ^:const +cached-words+
  '("aback" "apple" "head" "foo" "deal" "complement" "one" "two" "main"))

(deftest test-cached-search
  (binding [*cache-home* "dev-resources/cache"]
    (doseq [w +cached-words+]
      (is (.exists (local-path :collegiate :xml w)))
      (validate-word w))))

(deftest test-uncached-search
  (validate-word "asylum"))

(deftest test-failed-search
  (let [w "non-existing-word-foo-bar-42"]
    (is (nil? (search :collegiate :xml w)))
    (is (not (.exists (local-path :collegiate :xml w))))))
