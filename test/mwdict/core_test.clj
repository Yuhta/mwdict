(ns mwdict.core-test
  (:use clojure.test
        mwdict.core
        [clojure.java.io :only [resource]]
        [mwdict.text :only [render entry-found?]]))

(defn- validate-word [w]
  (is (= (render (search :collegiate :xml w))
         (slurp (resource (str "console-output/" w)))) w))

(def ^:const +cached-words+
  '("aback" "apple" "head" "foo" "deal" "complement" "one" "two" "main"
    "leg" "visual" "overcast" "stuck" "own" "42" "impasse" "plot" "exodus"
    "covert"))

(deftest test-cached-search
  (binding [*cache-home* "dev-resources/cache"]
    (doseq [w +cached-words+]
      (is (.exists (local-path :collegiate :xml w)))
      (validate-word w))))

(deftest test-uncached-search
  (validate-word "asylum"))

(deftest test-failed-search-no-suggestions
  (let [w "non-existing-word-foo-bar-42"
        t (search :collegiate :xml w)]
    (is (not (entry-found? t)))
    (is (= t "The word you've entered was not found. Please try your search again."))
    (is (not (.exists (local-path :collegiate :xml w))))))

(deftest test-failed-search-with-suggestions
  (let [w "ayslum"
        t (search :collegiate :xml w)]
    (is (not (entry-found? t)))
    (is (.contains (render t) "asylum"))
    (is (not (.exists (local-path :collegiate :xml w))))))
