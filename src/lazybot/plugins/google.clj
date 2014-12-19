(ns lazybot.plugins.google
  (:require [lazybot.registry :refer [defplugin send-message]]
            [lazybot.utilities :refer [trim-string]]
            [cheshire.core :refer [parse-string]] 
            [clojure.string :as s]
            [clj-http.client :as http]
            [lazybot.plugins.title :as t]
            [clojure.java.io :refer [reader]]
            [clojail.core :refer [thunk-timeout]])
  (:import java.util.concurrent.TimeoutException
  	  org.apache.commons.lang.StringEscapeUtils
           java.net.URLDecoder))

(defn search-string
  "From an argument list, builds a search string."
  [args]
  (->> args
       (s/join "%20")
       s/trim))

(defn handle-search [{:keys [com nick bot user channel message] :as com-m}]
	(let [q (search-string (:args com-m))]
                  (if-not (seq q)
                    (str "No search terms!")
                    (if (not (contains? (get-in @bot [:config (:server @com) :title :blacklist])
                          channel))
                            (let [link 
                            	    (with-open [readerurl (clojure.java.io/reader (str "https://duckduckgo.com/?q=\\" q))]
                            	    	    (java.net.URLDecoder/decode (second
                            	    	    		    (re-find #"uddg=(.+?)'" (apply str (line-seq readerurl))))))]
      (try
       (thunk-timeout #(let [url (t/add-url-prefix link)
                             page (t/slurp-or-default url)
                             match (second page)]
                         (if (and (seq page) (seq match) (not (t/url-check com bot url)))
                           (send-message com-m
                                              (apply str (take 200 (str link " \""
                                                   (s/triml
                                                    (StringEscapeUtils/unescapeHtml
                                                     (t/collapse-whitespace match)))
                                                   "\""))))
                           ))
                      20 :sec)
       (catch TimeoutException _
           (println "It's taking too long to do the search. I'm giving up."))))))))

#_(defn handle-search-old [com-m]
  (send-message com-m
                (let [q (search-string (:args com-m))]
                  (if-not (seq q)
                    (str "No search terms!")
                    (let [results (google "web" q)
                          {:strs [url titleNoFormatting]}
                            (first (get-in results ["responseData" "results"]))
                          res-count (get-in results ["responseData"
                                                     "cursor"
                                                     "estimatedResultCount"])]
                      (if (and results url)
                        (str "["
                             (trim-string 80 (constantly "...")
                                          (StringEscapeUtils/unescapeHtml
                                            titleNoFormatting))
                             "] "
                             (URLDecoder/decode url "UTF-8"))))))))

(defplugin
  (:cmd
   "Searches google for whatever you ask it to, and displays the first result
   and its title."
   #{"google" "g"}
   #'handle-search)

  (:cmd
   "Searches wikipedia via google."
   #{"wiki" "w"}
   (fn [args]
     (handle-search (assoc args :args (conj (:args args) "site:en.wikipedia.org")))))

  (:cmd
   "Searches YouTube via google."
   #{"youtube" "yt"}
   (fn [args]
     (handle-search (assoc args :args (conj (:args args) "site:youtube.com"))))))

