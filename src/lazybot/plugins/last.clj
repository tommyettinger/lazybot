(ns lazybot.plugins.last
  (:use [lazybot registry info]
        [lazybot.utilities :only [format-time]]
	[somnium.congomongo :only [fetch fetch-one insert!]]
       ; [clojure.pprint :only [pprint]]

	[clojure.string :only [join split lower-case replace]])
  (:use [clucy.core :as clucy])
  (:import [java.util.regex Pattern]))
(def surch (disk-index "surch"))
(defn now []
  (System/currentTimeMillis))
(comment
(defn make-keyword-lower
	"Helper function for making keywords"
	[s]
	(keyword (lower-case s))
	)
)
(defn tack-time
  "Takes a nick and updates the last database with that nick and the current time."
  [nick server channel message]
  
    (clucy/add surch
             (with-meta {
              :time (now)
              :chan channel
              :nick nick
              :message message } {:nick {:indexed false}}))
    (insert! :last
             {:server server
              :time (now)
              :chan channel 
              :message message
              :nick nick}))

(defn get-last
  "Gets the last statement."
  [server]
  (when-let [last-map (first (fetch :last :limit 500 :sort {:time -1} :where {:server server}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from
  "Gets the last statement for a nick."
  [server nick]
  (when-let [last-map (first (fetch :last :limit 500 :sort {:time -1} :where {:nick nick :server server}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-in
  "Gets the last-in for a channel"
  [server chan]
  (when-let [last-map (first (fetch :last :limit 100 :sort {:time -1} :where {:server server :chan chan}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-in
  "Gets the last-in for a nick."
  [server nick chan]
  (when-let [last-map (first (fetch :last :limit 100 :sort {:time -1} :where {:nick nick :server server :chan chan}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-with
  "Gets the last statement from a nick with the given text."
  [server nick with-text]
  ;(prn first (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)))
  (when-let [last-map (first (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)))]
  	  
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))

(defn get-last-from-with-in
  "Gets the last statement from a nick with the given text in a channel."
  [server nick with-text chan]
  ;(prn first (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)))
  (when-let [last-map (first (filter #(and (= (:nick %) nick) (= (:chan %) chan) ) (clucy/search surch with-text 300)))]
  	  
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))

(defn get-last-with
  "Gets the last statement with the given text."
  [server with-text]
  (when-let [last-map (first (clucy/search surch with-text 1))]
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))


(defn get-last-n
  "Gets the nth-to-last statement."
  [server n]
  (when-let [last-map (nth (fetch :last :limit n :sort {:time -1} :where {:server server}) (dec n))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-n
  "Gets the nth-to-last statement for a nick."
  [server nick n]
  (when-let [last-map (nth (fetch :last :limit n :sort {:time -1} :where {:nick nick :server server}) (dec n))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-in-n
  "Gets the nth-to-last-in for a channel"
  [server chan n]
  (when-let [last-map (nth (fetch :last :sort {:time -1} :where {:server server :chan chan} :limit n) (dec n))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-in-n
  "Gets the nth-to-last-in for a nick and a channel."
  [server nick chan n]
  (when-let [last-map (nth (fetch :last :sort {:time -1} :where {:nick nick :server server :chan chan} :limit n) (dec n))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-with-n
  "Gets the nth-to-last statement from a nick with the given text."
  [server nick with-text n]
  ;(prn first (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)))
  (when-let [last-map (nth (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)) (dec n))]
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))

(defn get-last-with-n
  "Gets the nth-to-last statement with the given text."
  [server with-text n]
  (when-let [last-map (nth (clucy/search surch with-text n) (dec n))]
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))



(defn get-last-rand
  "Gets a random statement from the latest 1000."
  [server]
  (when-let [last-map (rand-nth (fetch :last :limit 1000 :sort {:time -1} :where {:server server}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-rand
  "Gets a random statement from the latest 1000 for a nick."
  [server nick]
  (when-let [last-map (rand-nth (fetch :last :limit 1000 :sort {:time -1} :where {:nick nick :server server}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-in-rand
  "Gets a random statement from the latest 1000 for a channel"
  [server chan]
  (when-let [last-map (rand-nth (fetch :last :sort {:time -1} :where {:server server :chan chan} :limit 1000))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-in-rand
  "Gets a random statement from the latest 1000 for a nick."
  [server nick chan]
  (when-let [last-map (rand-nth (fetch :last :sort {:time -1} :where {:nick nick :server server :chan chan} :limit 1000))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-last-from-with-rand
  "Gets a random statement from the latest 1000 with the given text."
  [server nick with-text]
  ;(prn first (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)))
  (when-let [last-map (rand-nth (filter #(= (:nick %) nick) (clucy/search surch with-text 1000)))]
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))

(defn get-last-with-rand
  "Gets a random statement with the given text."
  [server with-text]
  (when-let [last-map (rand-nth (clucy/search surch with-text 1000))]
    (update-in last-map [:time] #(- (now) (Long/parseLong %)))))


(defn get-first-from
  "Gets the first statement known from a nick."
  [server nick]
  (when-let [last-map (first (fetch :last :limit 100 :sort {:time 1} :where {:nick nick :server server}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-first-in
  "Gets the first-in for a channel"
  [server chan]
  (when-let [last-map (first (fetch :last :limit 100 :sort {:time 1} :where {:server server :chan chan}))]
    (update-in last-map [:time] #(- (now) %))))

(defn get-first-from-in
  "Gets the first-in for a nick."
  [server nick chan]
  (when-let [last-map (first (fetch :last :limit 100 :sort {:time 1} :where {:nick nick :server server :chan chan}))]
    (update-in last-map [:time] #(- (now) %))))

(defn put-last [{:keys [nick channel com message]}] (tack-time nick (:server @com) channel message))

(defplugin
  (:hook :privmsg
         (fn [{:keys [com nick channel message] :as irc-map}] (if (re-find #"^[`!]" message)
         		 nil
         		 (put-last irc-map)
         		 )))
  (comment (:hook :join 
         (fn [irc-map] (put-last irc-map "joining")))
  (:hook :on-quit
         (fn [irc-map] (put-last irc-map "quitting"))))
  
  (:cmd
   "What just happened?."
   #{"last"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last (:server @com))]
       
                       (str "The latest message was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "What just happened?"))))
  
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastin" "last-in"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[chn] args]
       (send-message com-m
                     (if-let [{:keys [time nick chan message]}
                              (get-last-in (:server @com) chn)]
                         (str "The latest message in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "Nothing happened there (that I know of).")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastfrom" "last-from"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan message]}
                              (get-last-from (:server @com) who)]
       
                       (str "The latest message from " who " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
  (:cmd
   "Checks to see when the person you specify was last seen in a specified channel."
   #{"lastfromin" "last-from-in"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [who (first args)
     	     chan (nth args 2)]
       (send-message com-m
                     (if-let [{:keys [time chanl message]}
                              (get-last-from-in (:server @com) who chan)]
       
                       (str "The latest message from " who " was in "
                            chanl " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
   
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastwith" "last-with"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [with-text (replace (first args) #"\s" "")]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-with (:server @com) with-text)]
                         (str "The latest message with the text \""
                         	 with-text "\" was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "I've never heard that.")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastfromwith" "last-from-with"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [from-nick (first args)
     	   with-text (replace (nth args 1) #"\s" "")]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-from-with (:server @com) from-nick with-text)]
                         (str "The latest message with the text \""
                         	 with-text "\" from " from-nick " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       "I've never heard that.")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastfromwithin" "last-from-with-in"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [from-nick (first args)
     	   with-text (replace (nth args 1) #"\s" "")
     	   chan (nth args 2)]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-from-with-in (:server @com) from-nick with-text chan)]
                         (str "The latest message with the text \""
                         	 with-text "\" from " from-nick " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       "I've never heard that.")))))
  
  (:cmd
   "What just happened?"
   #{"rand-last" "randlast"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-rand (:server @com))]
       
                       (str "A random latest message was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "What just happened?"))))
  
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"randlastin" "rand-last-in"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[chn] args]
       (send-message com-m
                     (if-let [{:keys [time nick chan message]}
                              (get-last-in-rand (:server @com) chn)]
                         (str "A random message in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "Nothing happened there (that I know of).")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"randlastfrom" "rand-last-from"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan message]}
                              (get-last-from-rand (:server @com) who)]
       
                       (str "A random message from " who " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
   
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"randlastwith" "rand-last-with"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [with-text (replace (first args) #"\s" "")]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-with-rand (:server @com) with-text)]
                         (str "A random message with the text \""
                         	 with-text "\" was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "I've never heard that.")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"randlastfromwith" "rand-last-from-with"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [from-nick (first args)
     	   with-text (replace (nth args 1) #"\s" "")]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-from-with-rand (:server @com) from-nick with-text)]
                         (str "A random message with the text \""
                         	 with-text "\" from " from-nick " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       "I've never heard that.")))))
  
  
  
  (:cmd
   "What just happened?."
   #{"lastn" "last-n"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [n (Long/parseLong (first args))]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-n (:server @com) n)]
       
                       (str "The nth message was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "What just happened?")))))
  
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastinn" "last-in-n"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [chn (first args)
     	     n (Long/parseLong (nth args 1))]
       (send-message com-m
                     (if-let [{:keys [time nick chan message]}
                              (get-last-in-n (:server @com) chn n)]
                         (str "The nth message in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "Nothing happened there (that I know of).")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastfromn" "last-from-n"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [who (first args)
     	     n (Long/parseLong (nth args 1))]
       (send-message com-m
                     (if-let [{:keys [time chan message]}
                              (get-last-from-n (:server @com) who n)]
       
                       (str "The nth message from " who " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
  (:cmd
   "Checks the nth-latest message from the person you specify in a specified channel."
   #{"lastfrominn" "last-from-in-n"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [who (first args)
     	     chan (nth args 1)
     	     n (Long/parseLong (nth args 2))]
       (send-message com-m
                     (if-let [{:keys [time chan message]}
                              (get-last-from-in-n (:server @com) who chan n)]
       
                       (str "The nth message from " who " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
   
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastwithn" "last-with-n"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [with-text (replace (first args) #"\s" "")
     	     n (Long/parseLong (nth args 1))]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-with-n (:server @com) with-text n)]
                         (str "The nth message with the text \""
                         	 with-text "\" was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "I've never heard that.")))))
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"lastfromwithn" "last-from-with-n"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [from-nick (first args)
     	   with-text (replace (nth args 1) #"\s" "")
     	   n (Long/parseLong (nth args 2))]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-last-from-with-n (:server @com) from-nick with-text n)]
                         (str "The nth message with the text \""
                         	 with-text "\" from " from-nick " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       "I've never heard that.")))))
  
  (:cmd
   "What was the earliest recorded message in this channel?"
   #{"firstin" "first-in"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[chn] args]
       (send-message com-m
                     (if-let [{:keys [time nick chan message]}
                              (get-first-in (:server @com) chn)]
                         (str "The first message in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " nick ": " message)
                       "Nothing happened there (that I know of).")))))
  
  (:cmd
   "Checks to see when the person you specify was first seen by this bot in any channel."
   #{"seen-first" "seenfirst" "firstfrom" "first-from"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan message]}
                              (get-first-from (:server @com) who)]
       
                       (str "The first message from " who " was in "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
  (:cmd
   "Checks to see when the person you specify was first seen by this bot in a specified channel."
   #{"firstfromin" "first-from-in"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [who (first args)
     	     chan (nth args 2)]
       (send-message com-m
                     (if-let [{:keys [time chanl message]}
                              (get-first-from-in (:server @com) who chan)]
       
                       (str "The first message from " who " was in "
                            chanl " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
  (:index [[:nick :server :chan] :unique false]))
