(ns lazybot.plugins.decide
  (:use  [lazybot registry]
         [lazybot.utilities :only [prefix]]
  	 [clojure.string :only [join split]]))

(defplugin
  (:cmd
   "Decide between comma-separated choices."
   #{"decide"}
   (fn [{:keys [bot nick channel args] :as com-m}]
     (let [choices (clojure.string/split (join " " args) #", ?")]
     	     (println (str (str choices) "     " (apply str choices)))
       (send-message com-m (first (shuffle choices)))))))
