(ns lazybot.plugins.quote
  (:use [lazybot registry info]
        [lazybot.utilities :only [format-time]]
        [lazybot.plugins.seen :only [get-seen]]
        [lazybot.plugins.last :only [ get-last get-last-from
           get-last-in get-last-from-in get-last-from-with get-last-from-with-in
           get-last-with
           get-last-n get-last-from-n get-last-in-n get-last-from-in-n
           get-last-from-with-n get-last-with-n
          ]]
	[somnium.congomongo :only [fetch fetch-one insert! destroy!]]
        [clojure.string :only [join trim replace]]
        [lazybot.paste :only [trim-with-paste paste]]
        [clojure.pprint :only [pprint]]
        [lazybot.plugins.login :only [when-privs]])
  (:import java.io.StringWriter)
  )

(defn now []
  (System/currentTimeMillis))

(defn hashmap-to-string [m] 
  (let [w (StringWriter.)] (pprint m w)(.toString w)))

(defn tack-grab
  "Takes a nick and a message and updates the quote database with that nick, the message, and the current time."
  [nick server channel message]
  (let [lower-nick (.toLowerCase nick)]
    ;(destroy! :seen {:nick nick :server server})
    (insert! :quote
             {:server server
              :time (now)
              :chan channel 
              :message message
              :nick nick})))

(defn get-quote
  "Gets the last quote for a nick."
  [nick server]
  (when-let [seen-map (first (drop-while #(= (:message % ) "\tINVALID QUOTE") (fetch :quote :where {:nick nick :server server} :sort {:time -1})))]
    seen-map))
(defn get-barf
  "Gets ALL QUOTES."
  [server]
  (when-let [seen-map-seq (remove #(= (:message % ) "\tINVALID QUOTE")
  		  (fetch :quote :where {:server server} :sort {:time 1}))]
    (map #(str "In " (:chan %) ": " (:message %) " -- " (:nick %)) seen-map-seq)))
(defn get-barf-nick
  "Gets ALL QUOTES (by a person)."
  [server nick]
  (when-let [seen-map-seq (remove #(= (:message % ) "\tINVALID QUOTE")
  		  (fetch :quote :where {:nick nick :server server} :sort {:time 1}))]
    (map #(str "In " (:chan %) ": " (:message %) " -- " (:nick %)) seen-map-seq)))
(defn get-random
  "Gets a random quote for a nick."
  [nick server]
  (when-let [seen-map (first  (drop-while #(= (:message % ) "\tINVALID QUOTE") (shuffle (fetch :quote :where {:nick nick :server server}))))]
    seen-map))
(defn get-chaos
  "Gets a random quote for a nick."
  [server]
  (when-let [seen-map (first  (drop-while #(= (:message % ) "\tINVALID QUOTE") (shuffle (fetch :quote :where {:server server}))))]
    seen-map))

(defn put-quote [nick channel com message] (tack-grab nick (:server @com) channel message))

(defplugin 
  (:cmd
   "Puts the last message by a nick into the quote DB."
   #{"grab"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (put-quote who channel com (or (:message (get-last-from-in (:server @com) who channel)) "\tINVALID QUOTE"))
       (send-message com-m
       	       "Done."))))
  (:cmd
   "Puts the last message by a nick with a word into the quote DB."
   #{"grab-with" "grabwith"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [who (first args)
     	   with-text (replace (nth args 1) #"\s" "")
     	   msg (:message (get-last-from-with-in (:server @com) who with-text channel))]
       (if msg (do
       	       (put-quote who channel com msg)
       (send-message com-m
       	       "Done."))
       (send-message com-m
       	       "No such message."))
       	       )))
  (:cmd
   "Puts the nth message by a nick into the quote DB."
   #{"grab-n" "grabn"} 
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [who (first args)
     	     n (Long/parseLong (nth args 1))]
       (put-quote who channel com (or (:message (get-last-from-in-n (:server @com) who channel n)) "\tINVALID QUOTE"))
       (send-message com-m
       	       "Done."))))
  (:cmd
   "Gets the last quote by the given nick."
   #{"quote"}
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-quote who (:server @com))]
                              (apply str (take 400 (str "In " chan ", " "[" (trim (subs (.toGMTString (java.util.Date. time)) 0 11))
                                          "] " nick ": " (apply str (map #(char (min (int %) (int \~))) message)))))
                              (str "I have no quotes for " who "."))
   ))))
  (:cmd
   "Gets a random quote by the given nick."
   #{"random"}
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
         (if (= 0 (count args))
              (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-chaos (:server @com))]
                              (apply str (take 400 (str "In " chan ", " "[" (trim (subs (.toGMTString (java.util.Date. time)) 0 11))
                                          "] " nick ": " (apply str (map #(char (min (int %) (int \~))) message)))))
                              (str "I have no quotes for some reason.")))
              
      	      (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-random who (:server @com))]
                              (apply str (take 400 (str "In " chan ", " "[" (trim (subs (.toGMTString (java.util.Date. time)) 0 11))
                                          "] " nick ": " (apply str (map #(char (min (int %) (int \~))) message)))))
                              (str "I have no quotes for " who ".")))))))
  (:cmd
   "Gets a random quote by anyone."
   #{"chaos"}
   (fn [{:keys [com bot channel args] :as com-m}]
     (let [[who] args]
       (send-message com-m
                     (if-let [{:keys [time chan nick message]}
                              (get-chaos (:server @com))]
                              (apply str (take 400 (str "In " chan ", " "[" (trim (subs (.toGMTString (java.util.Date. time)) 0 11))
                                          "] " nick ": " (apply str (map #(char (min (int %) (int \~))) message)))))
                              (str "I have no quotes for some reason."))
   ))))
  
  (:cmd
   "Super-Barf out every quote onto refheap."
   #{"superbarf"}
   (fn [{:keys [com bot nick args channel] :as com-m}]
     (send-message com-m
                             (trim-with-paste 100 "Plain Text" "" (join "\n" (get-barf (:server @com)))))))
  (:cmd
   "Super-Barf out every quote by a given user onto refheap."
   #{"stalk" "barfnick"}
   (fn [{:keys [com bot nick args channel] :as com-m}]
     (let [[who] args]
         (if (= 0 (count args))
         	 (let [m (trim-with-paste 100 "Plain Text" "" (join "\n" (get-barf (:server @com))))]
         	 	 (send-message com-m
                             (replace m #"\n" "  ")))
         	 (let [m (trim-with-paste 100 "Plain Text" "" (join "\n" (get-barf-nick (:server @com) who)))]
         	 	 (send-message com-m
                             (replace m #"\n" "  ")))
                 ))))

  (:index [[:nick :server] :unique false]))
