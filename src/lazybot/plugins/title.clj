;; The result of a team effort between programble and Rayne.
(ns lazybot.plugins.title
  (:use [lazybot info registry utilities])
  (:require 
        [clojure.java.io :refer [reader]]
        [clojure.string :as s]
        [clojure.tools.logging :refer [debug]]
        [clojail.core :refer [thunk-timeout]])
  (:import java.util.concurrent.TimeoutException
           org.apache.commons.lang.StringEscapeUtils))

(def titlere #"(?i)<title>([^<]+)</title>")

(defn collapse-whitespace [s]
  (->> s (.split #"\s+") (interpose " ") (apply str)))

(defn add-url-prefix [url]
  (cond
    (not (.startsWith url "http"))
    (str "https://" url)
    (re-find #"http://(www\.)?youtu" url)
    (s/replace url #"http:" "https:")
    :else
    url))

(defn slurp-or-default [url]
  (try
   (with-open [readerurl (reader url)]
     (loop [acc [] lines (line-seq readerurl)]
       (cond
        (not (seq lines)) nil
        (some #(re-find #"</title>|</TITLE>" %) acc) (->> acc (apply str)
                                                          (#(.replace % "\n" " "))
                                                          (re-find titlere))
        :else (recur (conj acc (first lines)) (rest lines)))))
   (catch java.lang.Exception e nil)))

(defn url-blacklist-words [com bot] (:url-blacklist ((:config @bot) (:server @com))))

(defn url-check [com bot url]
  (some #(.contains url %) (url-blacklist-words com bot)))

(defn strip-tilde [s] (apply str (remove #{\~} s)))

(defn title [{:keys [com nick bot user channel] :as com-m}
             links & {verbose? :verbose?}]
  (if (or (and verbose? (seq links))
          (not (contains? (get-in @bot [:config (:server @com) :title :blacklist])
                          channel)))
    (doseq [link (take 1 links)]
      (try
       (thunk-timeout #(let [url (add-url-prefix link)
                             page (slurp-or-default url)
                             match (second page)]
                         (if (and (seq page) (seq match) (not (url-check com bot url)))
                           (send-message com-m
                                              (str "\""
                                                   (s/triml
                                                    (StringEscapeUtils/unescapeHtml
                                                     (collapse-whitespace match)))
                                                   "\""))
                           (when verbose? (send-message com-m "Page has no title."))))
                      20 :sec)
       (catch TimeoutException _
         (when verbose?
           (send-message com-m "It's taking too long to find the title. I'm giving up.")))))
    (when verbose? (send-message com-m "Which page?"))))

(defplugin
  (:hook
   :privmsg
   (fn [{:keys [network bot nick channel message] :as com-m}]
     (let [info (:config @bot)
           get-links (fn [s]
                       (->> s
                            (re-seq #"(https?://|www\.)[^\]\[(){}\"'$^\s]+")
                            (map first)))]
       (let [prepend (:prepends info)
             links (get-links message)
             title-links? (and (not (is-command? message prepend))
                               ; (get-in info [(:server @com) :title :automatic?])
                               (seq links)
                               (re-find #"youtu" (first links)))]
         
         (when title-links?
           (title com-m links :verbose? false))))))

  (:cmd
   "Gets the title of a web page. Takes a link. This is verbose, and prints error messages."
   #{"title"} (fn [com-m] (title com-m (:args com-m) :verbose? true))))
