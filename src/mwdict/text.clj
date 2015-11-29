(ns mwdict.text
  (:use [clojure.string :only [upper-case]])
  (:import (org.fusesource.jansi Ansi Ansi$Attribute)))

(defn- a [^Ansi buf x] (.a buf x))
(defn- bold [^Ansi buf] (.bold buf))
(defn- italic [^Ansi buf] (.a buf Ansi$Attribute/ITALIC))
(defn- reset [^Ansi buf] (.reset buf))

(defmulti comma-rule #(vector (type %1) (type %2)))
(defmethod comma-rule :default [_ _] false)

(defn followed-by-space [x] (assoc x :followed-by " "))
(defn preceded-by-space [x] (assoc x :preceded-by " "))

(defprotocol Text
  (render-to-ansi [self buf])
  (update-string [self f]))

(defn ^String render
  "Render Text t to a String"
  [t & {:keys [dumb]}]
  (let [enabled (Ansi/isEnabled)
        buf (try
              (Ansi/setEnabled (not dumb))
              (Ansi/ansi)
              (finally (Ansi/setEnabled enabled)))]
    (render-to-ansi t buf)
    (str buf)))

(defmulti parse
  "Parse a source (File or InputStream) into Text"
  (fn [reference fmt _] [reference fmt]))

(defn entry-found? [t] (:entry-found t))
(defn entry-found [t] (assoc t :entry-found true))

(defn- separator [x y]
  (let [suf (:followed-by x)
        pre (:preceded-by y)]
    (cond
      suf (cond
            (comma-rule x y) ", "
            (let [suf-str (render suf :dumb true)]
              (.startsWith (render y :dumb true) suf-str)) nil
            :else suf)
      pre (let [pre-str (render pre :dumb true)]
            (when-not (.endsWith (render x :dumb true) pre-str)
              pre)))))

(extend-type String
  Text
  (render-to-ansi [this buf] (a buf this))
  (update-string [this f] (f this)))

(extend-type nil
  Text
  (render-to-ansi [_ _])
  (update-string [_ _]))

(defrecord Join [sep texts]
  Text
  (render-to-ansi [_ buf]
    (when-let [t (first texts)]
      (render-to-ansi t buf)
      (doseq [[x y] (partition 2 1 texts)]
        (render-to-ansi (or sep (separator x y)) buf)
        (render-to-ansi y buf))))
  (update-string [this f]
    (update this :texts (partial map #(update-string % f)))))

(defmacro def-singleton-text [record-name [field-name] & body]
  `(defrecord ~record-name [~field-name]
     Text
     (update-string [this# f#]
       (update this# ~(keyword (name field-name))
               #(update-string % f#)))
     ~@body))

(def-singleton-text Boldface [text]
  (render-to-ansi [_ buf]
    (bold buf)
    (render-to-ansi text buf)
    (reset buf)))

(def-singleton-text Italic [text]
  (render-to-ansi [_ buf]
    (italic buf)
    (render-to-ansi text buf)
    (reset buf)))

(defmethod comma-rule [Italic Italic] [_ _] true)

(def-singleton-text SmallCaps [text]
  (render-to-ansi [_ buf]
    (render-to-ansi (update-string text upper-case) buf)))

(defmethod comma-rule [SmallCaps SmallCaps] [_ _] true)

(def-singleton-text EmDash [text]
  (render-to-ansi [_ buf]
    (render-to-ansi "\u2014 " buf)
    (render-to-ansi text buf)))

(def-singleton-text CommaRule [text]
  (render-to-ansi [_ buf] (render-to-ansi text buf)))

(defmethod comma-rule [CommaRule CommaRule] [_ _] true)
