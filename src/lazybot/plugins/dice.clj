(ns lazybot.plugins.dice
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
   "Roll dice.  Uses format ?d#, where ? is number of dice
   and # is the sides on those dice."
   #{"dice" "roll"}
   (fn [{:keys [bot nick channel args] :as com-m}]
     (let [xdx (split (nth args 0) #"d")
     	   dice (map #(inc (rand-int %)) (repeat (read-string (nth xdx 0)) (read-string (nth xdx 1))))
     	   ]
     	      
       (send-message com-m (apply str (take 220 (str (clojure.string/join " " dice) "  Total: " (apply + dice)))))))))
