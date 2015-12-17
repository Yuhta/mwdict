(ns mwdict.collegiate-xml
  (:require [clojure.xml :as xml])
  (:use mwdict.text
        [clojure.string :only [escape split triml]]
        [clojure.set :only [union]]))

(defmulti ^:private ->text #(:tag %))

(defn- coll->text
  ([xs] (coll->text xs nil))
  ([xs sep]
   (if (and (vector? xs) (= (count xs) 1))
     (->text (first xs))
     (->Join sep (map ->text xs)))))

(defmethod parse [:collegiate :xml] [_ _ src]
  (let [content (group-by :tag (:content (xml/parse src)))]
    (assert (not (next (keys content))))
    (case (first (keys content))
      :entry (entry-found (coll->text (:entry content) "\n\f\n"))
      :suggestion (->> (coll->text (:suggestion content) "\n")
                       (list (str "The word you've entered isn't in the dictionary. "
                                  "Choose a spelling suggestion below or try again."))
                       (->Join "\n\n"))
      nil "The word you've entered was not found. Please try your search again.")))

(defn- content->text [node & opts]
  (apply coll->text (:content node) opts))

(def ^:const +superscript-table+
  (into {\0 \u2070, \1 \u00B9, \2 \u00B2, \3 \u00B3}
        (for [n (range 4 10)]
          [(char (+ (int \0) n)) (char (+ 0x2070 n))])))

(def ^:const +subscript-table+
  (into {}
        (for [n (range 10)]
          [(char (+ (int \0) n)) (char (+ 0x2080 n))])))

(defn- superscript [n] (when n (escape n +superscript-table+)))
(defn- subscript [n] (when n (escape n +subscript-table+)))

(defn- headword [node]
  (->Boldface (escape (apply str
                             (superscript (:hindex (:attrs node)))
                             (:content node))
                      {\* \u00B7})))

(defmethod ->text nil [node]
  (assert (string? node))
  node)

(doseq [tag '(:fw :sxn :ctn :dxn :snp :pt :suggestion)]
  (defmethod ->text tag [node]
    (content->text node)))

(defmethod ->text :d_link [node]
  (followed-by-space (->Wrapped (content->text node))))

(defmethod ->text :g [node]
  (followed-by-space (->Join nil (list (content->text node)))))

(doseq [tag '(:sn :va)]
  (defmethod ->text tag [node]
    (followed-by-space (->Boldface (content->text node)))))

(defmethod ->text :it [node]
  (->Italic (content->text node)))

(doseq [tag '(:fl :lb :vt :sl :ssl :il :vl :cl :spl :cat)]
  (defmethod ->text tag [node]
    (followed-by-space (->Italic (content->text node)))))

(defmethod ->text :sc [node]
  (assoc (->SmallCaps (content->text node)) :followed-by ""))

(defmethod ->text :pl [node]
  (followed-by-space (->Italic (->Boldface (content->text node)))))

(defmethod ->text :inf [node]
  (subscript (apply str (:content node))))

(doseq [[target target-number] [[:sx :sxn] [:ct :ctn] [:dxt :dxn]]]
  (defmethod ->text target [node]
    (let [[ts tns] (map coll->text
                        (split-with #(not= (:tag %) target-number)
                                    (:content node)))]
      (-> (->Join nil (list (->SmallCaps ts) tns))
          ->CommaRule
          followed-by-space))))

(doseq [tag '(:dx :un :aq :ca)]
  (defmethod ->text tag [node]
    (preceded-by-space (->EmDash (content->text node)))))

(defmethod ->text :ure [node]
  (->EmDash (headword node)))

(defmethod ->text :drp [node]
  (->EmDash (->Boldface (content->text node))))

(defmethod ->text :ss [node]
  (->Join " " (list (->Boldface (->Italic "syn")) "see"
                    (->SmallCaps (content->text node)))))

(defmethod ->text :us [node]
  (->Join " " (list (->Boldface (->Italic "usage")) "see"
                    (->SmallCaps (content->text node)))))

(doseq [tag '(:cx)]
  (defmethod ->text tag [node]
    (content->text node " ")))

(doseq [tag '(:in :vr :svr :uro)]
  (defmethod ->text tag [node]
    (-> (remove (comp #{:sound} :tag) (:content node))
        (coll->text " ")
        followed-by-space)))

(defmethod ->text :sin [node]
  (followed-by-space (content->text node " ")))

(defmethod ->text :dro [node]
  (content->text node "\n"))

(doseq [tag '(:hw :if :item)]
  (defmethod ->text tag [node]
    (headword node)))

(doseq [tag '(:pr :sp)]
  (defmethod ->text tag [node]
    (followed-by-space (->Join nil (list "\\" (content->text node) "\\")))))

(defmethod ->text :vi [node]
  (preceded-by-space (->Join nil (list "<" (content->text node) ">"))))

(defmethod ->text :sd [node]
  (followed-by-space (->Join " " (list ";" (->Italic (content->text node))))))

(defmethod ->text :frac [node]
  (let [[p q] (split (apply str (:content node)) #"/")]
    (str (superscript p) \u2044 (subscript q))))

(defmethod ->text :dt [node]
  (->Join nil (for [x (update (:content node) 0
                              #(if (string? %) (triml %) %))]
                (if (string? x)
                  (->Join (->Join nil (list (->Boldface ":") " "))
                          (split x #": ?" -1))
                  (->text x)))))

(defmethod ->text :def [node]
  (letfn [(add-new-sense [sections x]
            (conj sections (->Join nil [x])))
          (push-to-sense [sections x]
            (assert (vector? sections))
            (if (empty? sections)
              (add-new-sense sections x)
              (update-in sections [(dec (count sections)) :texts]
                         #(conj % x))))
          (collect [sections node]
            (case (:tag node)
              (:vt :sn :ss :us) (add-new-sense sections (->text node))
              (:sl :ssl :dt :sd :sin :svr :sp) (push-to-sense sections
                                                              (->text node))
              :date sections))]
    (->Join "\n\n" (reduce collect [] (:content node)))))

(defmethod ->text :list [node]
  (content->text node "\n"))

(def ^:const +general-body-tags+ #{:def :dro :uro :cx})
(def ^:const +paragraph-tags+ #{:pl :pt})
(def ^:const +body-tags+ (union +general-body-tags+ +paragraph-tags+))

(defmethod ->text :entry [node]
  (let [[head body] (->> (:content node)
                         (remove (comp #{:ew :subj :sound :et :grp :art} :tag))
                         (partition-by #(condp get (:tag %)
                                          #{:hw :vr :in} :>> identity
                                          +general-body-tags+ :body
                                          +paragraph-tags+ :pl
                                          :head))
                         (split-with #(not (+body-tags+ (:tag (first %))))))
        head-text (->Join "\n" (for [nodes head]
                                 (case (:tag (first nodes))
                                   :in (coll->text nodes "; ")
                                   (coll->text nodes))))
        body-texts (mapcat (fn [nodes]
                             (condp contains? (:tag (first nodes))
                               +paragraph-tags+ (map #(coll->text % " ")
                                                     (partition-all 2 nodes))
                               (map ->text nodes)))
                           body)]
    (->Join "\n\n" (list* head-text body-texts))))
