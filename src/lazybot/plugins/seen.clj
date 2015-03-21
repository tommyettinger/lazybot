(ns lazybot.plugins.seen
  (:use [lazybot registry info]
        [lazybot.utilities :only [format-time]]
	[somnium.congomongo :only [fetch fetch-one insert! destroy!]]
        [clojure.string :only [join]])
  (:import [java.util.regex Pattern]))

(defn now []
  (System/currentTimeMillis))

(defn tack-time
  "Takes a nick and updates the seen database with that nick and the current time."
  [nick server channel doing message]
  (let [lower-nick (.toLowerCase nick)]
    (destroy! :seen {:nick nick :server server :chan channel})
    (insert! :seen
             {:server server
              :time (now)
              :chan channel 
              :doing doing
              :nick nick
              :message message})))
(defn get-seen
  "Gets the last-seen for a nick."
  [nick server]
  (when-let [seen-map (first (fetch :seen :limit 4 :sort {:time -1} :where {:nick nick :server server}))]
    (update-in seen-map [:time] #(- (now) %))))

(defn get-seen-here
  "Gets the last-seen for a nick."
  [nick server chan]
  (when-let [seen-map (fetch-one :seen :where {:nick nick :server server :chan chan})]
    (update-in seen-map [:time] #(- (now) %))))

(defn get-seen-insen
  "Gets the last-seen for a nick."
  [nick server]
  (when-let [seen-map (fetch-one :seen :where {:nick (java.util.regex.Pattern/compile nick java.util.regex.Pattern/CASE_INSENSITIVE) :server server})]
    (update-in seen-map [:time] #(- (now) %))))

(defn put-seen [{:keys [nick channel com message]} doing] (tack-time nick (:server @com) channel doing message))

(defplugin
  (:hook :privmsg
         (fn [{:keys [com nick channel message] :as irc-map}] (if (re-find #"^[`!]" message)
         		 nil
         		 (put-seen irc-map "talking")
         		 )))
  (comment (:hook :join 
         (fn [irc-map] (put-seen irc-map "joining")))
  (:hook :quit
         (fn [irc-map] (put-seen irc-map "quitting"))))
  
  (:cmd
   "Checks to see when the person you specify was last seen."
   #{"seen"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan doing message]}
                              (get-seen who (:server @com))]
       
                       (str who " was last seen " doing
                            " on "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
  
  (:cmd
   "Checks to see when the person you specify was last seen in this channel."
   #{"seenhere" "seen-here"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan doing message]}
                              (get-seen-here who (:server @com) channel)]
       
                       (str who " was last seen " doing
                            " on "
                            chan " " (or (format-time time)
                                         "just moments") " ago: " message)
                       (str "I have never seen " who "."))))))
  (:index [[:nick :server :chan] :unique true]))
