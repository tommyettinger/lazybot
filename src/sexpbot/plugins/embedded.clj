(ns sexpbot.plugins.embedded
  (:use [sexpbot info respond]
        [clj-config.core :only [get-key]]))

(defplugin
  (:add-hook :on-message
             (fn [{message :message :as irc-map}]
               (doseq [x (reverse (re-seq #"\$#(.*?)#\$" message))]
                 (->> x second (str (get-key :prepend info-file))
                      (assoc irc-map :message) try-handle)))))