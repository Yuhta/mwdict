(ns mwdict.core
  (:require mwdict.collegiate-xml)
  (:use [clojure.string :exclude [reverse replace]]
        clojure.java.io
        mwdict.text)
  (:import (java.io File ByteArrayOutputStream)
           java.net.URLEncoder)
  (:gen-class))

(defn api-key [reference]
  (let [env (str "MWDICT_API_KEY_" (upper-case (name reference)))
        key (System/getenv env)]
    (assert key (str "cannot find API key (read from environment variable " env ")"))
    key))

(defn url [reference fmt word]
  (format "http://www.dictionaryapi.com/api/v1/references/%s/%s/%s?key=%s"
          (name reference)
          (name fmt)
          (URLEncoder/encode word "UTF-8")
          (api-key reference)))

(def ^:dynamic *cache-home*
  (or (System/getenv "MWDICT_CACHE_HOME")
      (file (or (System/getenv "XDG_CACHE_HOME")
                (file (System/getenv "HOME") ".cache"))
            "mwdict")))

(defn ^File local-path [reference fmt word]
  (let [dir (file *cache-home* (name reference))]
    (.mkdirs dir)
    (file dir (str word "." (name fmt)))))

(defn ->bytes [x]
  (with-open [in (input-stream x), out (ByteArrayOutputStream.)]
    (copy in out)
    (.toByteArray out)))

(defn search [reference fmt word]
  (let [path (local-path reference fmt word)]
    (if (.exists path)
      (parse reference fmt path)
      (let [bs (->bytes (url reference fmt word))
            parsed (parse reference fmt (input-stream bs))]
        (when (entry-found? parsed) (copy bs path))
        parsed))))

(defn -main [word]
  (println (render (search :collegiate :xml (lower-case (trim word))))))
