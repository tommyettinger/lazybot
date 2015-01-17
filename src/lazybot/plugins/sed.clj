(ns lazybot.plugins.sed
  (:use [lazybot.info]
        [lazybot.utilities :only [prefix]]
        [clojure.string :only [join replace split]]
        clojure.tools.logging
        [lazybot.registry :as registry]))

(def message-map (atom {}))
(def sed-regex #"^s/([^/]+)/([^/]*)/?")
(def ied-regex #"^I/([^/]+)/([^/]*)/?")

(defn- format-msg [{:keys [bot nick] :as com-m}]
  (send-message com-m (prefix nick "Format is sed [-<user name>] s/<regexp>/<replacement>/ Try <prefix>help sed")))

(defn sed* [string regexp replacement]
  (try
    (.replaceAll string (str "(?i)" regexp) replacement)))

(defn substitute [com-m verbose?]
  (let [{:keys [bot com nick args channel]} com-m
  	arg-str (.trim (join " " args))
        splot (split arg-str #"/" -1)
        commands (read-string (str "{" (.trim (join " " (mapv #(if (re-find #"\\|[\(\)\[\]]" %) (str "#\"" (replace % "\\" "\\") \") (str \" % \"))
        					(subvec splot 1 (dec (count splot)))))) "}"))        _ (println "commands: " commands)
        try-to-match (fn [regex]
                       (or (second (re-find regex arg-str))
                           ""))
        user-to (try-to-match #"^\s*-?([A-Za-z0-9\[\]\_\-\`\|]+)")
        margs (try-to-match #"\s*(s/[^/]+/[^/]*/?)$")
        orig-msg (some #(get (get-in @message-map [com channel])
                             %)
                       [nil :channel-last]) ; [user-to :channel-last])
        results (reduce #(replace %1 (first %2) (second %2))
        	orig-msg commands)
        [regexp replacement] (next (re-find sed-regex margs))]
    (cond
     (and verbose? (empty? orig-msg))
     (send-message com-m "No one said anything yet!")

     (and verbose? (not-any? seq [regexp replacement]))
     (format-msg com-m)

     :else
     (try
       (let [new-msg results]
         (when-not (= orig-msg new-msg)
           (send-message com-m (str "<" nick "> " new-msg))))
       (catch Exception _
         (when verbose? (format-msg com-m)))))))

(defn substitute-me [com-m verbose?]
  (let [{:keys [bot com nick args channel]} com-m
        arg-str (.trim (join " " args))
        splot (split arg-str #"/" -1)
        commands (read-string (str "{" (.trim (join " " (mapv #(if (re-find #"\\|[\(\)\[\]]" %) (str "#\"" (replace % "\\" "\\") \") (str \" % \"))
        					(subvec splot 1 (dec (count splot)))))) "}"))
        _ (println "commands: " commands)
        try-to-match (fn [regex]
                       (or (second (re-find regex arg-str))
                           ""))
        user-to (try-to-match #"^\s*-?([A-Za-z0-9\[\]\_\-\`\|]+)")
        margs (try-to-match #"\s*(s/[^/]+/[^/]*/?)$")
        orig-msg (some #(get (get-in @message-map [com channel])
                             %)
                       [nick :channel-last]) ; [user-to :channel-last])
        results (reduce #(replace %1 (first %2) (second %2))
        	orig-msg commands)
        [regexp replacement] (next (re-find sed-regex margs))]
    (cond
     (and verbose? (empty? orig-msg))
     (send-message com-m "No one said anything yet!")

     (and verbose? (not-any? seq [regexp replacement]))
     (format-msg com-m)

     :else
     (try
       (let [new-msg results]
         (when-not (= orig-msg new-msg)
           (send-message com-m (str "<" nick "> " new-msg))))
       (catch Exception _
         (when verbose? (format-msg com-m)))))))

(defn sed [com-m verbose?]
  (let [{:keys [bot com nick args channel]} com-m

        arg-str (.trim (join " " args))
        try-to-match (fn [regex]
                       (or (second (re-find regex arg-str))
                           ""))
        user-to (try-to-match #"^\s*-?([A-Za-z0-9\[\]\_\-\`\|]+)")
        margs (try-to-match #"\s*(s/[^/]+/[^/]*/?)$")
        orig-msg (some #(get (get-in @message-map [com channel])
                             %)
                       [nil :channel-last]) ; [user-to :channel-last])
        [regexp replacement] (next (re-find sed-regex margs))]
    (cond
     (and verbose? (empty? orig-msg))
     (send-message com-m "No one said anything yet!")

     (and verbose? (not-any? seq [regexp replacement]))
     (format-msg com-m)

     :else
     (try
       (let [new-msg (sed* orig-msg regexp replacement)]
         (when-not (= orig-msg new-msg)
           (send-message com-m (str "<" user-to "> " new-msg))))
       (catch Exception _
         (when verbose? (format-msg com-m)))))))


(defn ied [com-m verbose?]
  (let [{:keys [bot com nick args channel]} com-m

        arg-str (.trim (join " " args))
        try-to-match (fn [regex]
                       (or (second (re-find regex arg-str))
                           ""))
        user-to (try-to-match #"^\s*-?([A-Za-z0-9\[\]\_\-\`\|]+)")
        margs (try-to-match #"\s*(I/[^/]+/[^/]*/?)$")
        orig-msg (some #(get (get-in @message-map [com channel])
                             %)
                       [user-to :channel-last]) ; [nil :channel-last])
        [regexp replacement] (next (re-find ied-regex margs))]
    (cond
     (and verbose? (empty? orig-msg))
     (send-message com-m "No one said anything yet!")

     (and verbose? (not-any? seq [regexp replacement]))
     (format-msg com-m)

     :else
     (try
       (let [new-msg (sed* orig-msg regexp replacement)]
         (when-not (= orig-msg new-msg)
           (send-message com-m (str "<" user-to "> " new-msg))))
       (catch Exception _
         (when verbose? (format-msg com-m)))))))

(registry/defplugin
  (:hook
   :privmsg
   (fn [{:keys [com bot nick message channel] :as com-m}]
     (when (and (get-in @bot [:config :sed :automatic?])
                (not (when-let [blacklist (get-in @bot [:config (:server @com) :sed :blacklist])]
                       (blacklist channel))))
       (when (seq (re-find sed-regex message))
         (sed (assoc com-m :args [nick message]) false))
       (when (seq (re-find ied-regex message))
         (ied (assoc com-m :args [nick message]) false))
       (when (and (not= nick (:name @com))
                  (not (some
                        (-> @bot :config :prepends)
                        [(str (first message))])))
         (swap! message-map update-in [com channel]
                assoc nick message, :channel-last message))
       )))

  (:cmd
   "Simple find and replace. Usage: sed [-<user name>] s/<regexp>/<replacement>/
If the specified user isn't found, it will default to the last thing said in the channel.
Example Usage: sed -boredomist s/[aeiou]/#/
Shorthand : s/[aeiou]/#/"
   #{"sed"}
   (fn [com-m]
     (sed com-m true)))
  (:cmd
   "Multiple find and replace. Usage: sub [slash-separated list of /search/replace/search/replace/ strings]
Will substitute the last the last thing said in the channel by not-this-bot.
Example Usage: sub /a/A/e/E/i/I/o/O/u/U/"
#{"sub"}
   (fn [com-m]
     (substitute com-m false)))
  (:cmd
   "Multiple find and replace. Usage: subme [slash-separated list of /search/replace/search/replace/ strings]
Will substitute the last the last thing said in the channel by you, even if other people spoke in-between.
Example Usage: subme /a/A/e/E/i/I/o/O/u/U/"
   #{"subme"}
   (fn [com-m]
     (substitute-me com-m false)))
  )
