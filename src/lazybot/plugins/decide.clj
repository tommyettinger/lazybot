(ns lazybot.plugins.decide
  (:use  [lazybot registry]
         [lazybot.utilities :only [prefix]]
  	 [clojure.string :only [join split]]))
(comment
(def responses
     ["As I see it, yes."
      "It is certain."
      "It is decidedly so."
      "Most likely."
      "Outlook good."
      "Signs point to yes."
      "Without a doubt."
      "Yes."
      "Yes - definitely."
      "You may rely on it."
      "Reply hazy, try again."
      "Ask again later."
      "Better not tell you now."
      "Cannot predict now."
      "Concentrate and ask again."
      "Don't count on it."
      "My reply is no."
      "My sources say no."
      "Outlook not so good."
      "Very doubtful."]))

(defplugin
  (:cmd
   "Decide between comma-separated choices."
   #{"decide"}
   (fn [{:keys [bot nick channel args] :as com-m}]
     (let [choices (clojure.string/split (join " " args) #",")]
     	     (println (str (str choices) "     " (apply str choices)))
       (send-message com-m (first (shuffle choices)))))))
