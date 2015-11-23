(ns mwdict.collegiate-xml
  (:require [clojure.xml :as xml])
  (:use mwdict.text
        [clojure.string :only [escape split]]
        [clojure.set :only [union]]))

(defmulti ^:private ->text #(:tag %))

(defn- coll->text [xs & [sep]]
  (if (and (vector? xs) (= (count xs) 1))
    (->text (first xs))
    (->Join sep (map ->text xs))))

(defmethod parse [:collegiate :xml] [_ _ src]
  (let [entries (:content (xml/parse src))]
    (when-not (empty? entries)
      (coll->text entries "\n\f\n"))))

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

(doseq [tag '(:g :fw :sxn :ctn :dxn :snp :d_link :pt)]
  (defmethod ->text tag [node]
    (content->text node)))

(doseq [tag '(:sn :va)]
  (defmethod ->text tag [node]
    (followed-by-space (->Boldface (content->text node)))))

(defmethod ->text :it [node]
  (->Italic (content->text node)))

(doseq [tag '(:fl :lb :vt :sl :ssl :il :vl :cl)]
  (defmethod ->text tag [node]
    (followed-by-space (->Italic (content->text node)))))

(defmethod ->text :pl [node]
  (followed-by-space (->Italic (->Boldface (content->text node)))))

(doseq [[target target-number] [[:sx :sxn] [:ct :ctn] [:dxt :dxn]]]
  (defmethod ->text target [node]
    (let [[ts tns] (map coll->text
                        (split-with #(not= (:tag %) target-number)
                                    (:content node)))]
      (-> (->Join nil (list (->SmallCaps ts) tns))
          ->CommaRule
          followed-by-space))))

(doseq [tag '(:dx :un :aq)]
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

(doseq [tag '(:hw :if)]
  (defmethod ->text tag [node]
    (headword node)))

(defmethod ->text :pr [node]
  (followed-by-space (->Join nil (list "\\" (content->text node) "\\"))))

(defmethod ->text :vi [node]
  (preceded-by-space (->Join nil (list "<" (content->text node) ">"))))

(defmethod ->text :sd [node]
  (followed-by-space (->Join " " (list ";" (->Italic (content->text node))))))

(defmethod ->text :frac [node]
  (let [[p q] (split (apply str (:content node)) #"/")]
    (str (superscript p) \u2044 (subscript q))))

(defmethod ->text :dt [node]
  (->Join nil (for [x (:content node)]
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
              (:sl :ssl :dt :sd :sin :svr) (push-to-sense sections
                                                          (->text node))
              :date sections))]
    (->Join "\n\n" (reduce collect [] (:content node)))))

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
