(ns lazybot.run
  (:require [lazybot.core :as lazybot]
            [lazybot.irc :as irc]
            [lazybot.info :as info]
            [clojure.tools.cli :as cli]
            [clojure.java.io :refer [writer file]]
            [overtone.at-at :as aa])
  (:gen-class))
;; CHANGE THIS ON NON-WINDOWS PLATFORMS
(def reboot-string "cmd /c start cmd /c java  -jar  lazybot.jar")
(defn -main [& args]
  (let [{:keys [logpath background config-dir]}
        (cli/cli args
                 (cli/optional
                  ["--background"
                   "Start lazybot in the background. Should only be used along with --logpath."])
                 (cli/optional ["--logpath" "A file for lazybot to direct output to."])
                 (cli/optional ["--config-dir" "Directory to look for config.clj and other configuraiton."]))]
    (when config-dir
      (alter-var-root #'info/*lazybot-dir* (constantly (file config-dir))))
    (if background
      (.exec (Runtime/getRuntime)
             (str "java -jar lazybot.jar --logpath " logpath))
      (let [apool (aa/mk-pool)
      	    config (info/read-config)
      	    writr (if logpath (writer logpath) *out*)
      	    wrapt (proxy [java.io.BufferedWriter] [writr]
      	    	    (write ([cbuf off len] (do
      	    	    		    (aa/stop-and-reset-pool! apool :strategy :kill)
      	    	    		    (aa/every 360000 #(do
      	    	    		    		    (.exec (Runtime/getRuntime) reboot-string)
      	    	    		    		    (.println System/out "RECONNECTING")
      	    	    		    		    (System/exit 0))
      	    	    		    	    apool :initial-delay 360000)
      	    	    		    (proxy-super write cbuf off len)))
      	    	           ([c] (do
      	    	    		    (aa/stop-and-reset-pool! apool :strategy :kill)
      	    	    		    (aa/every 360000 #(do
      	    	    		    		    (.exec (Runtime/getRuntime) reboot-string)
      	    	    		    		    (.println System/out "RECONNECTING")
                                                    (System/exit 0))
      	    	    		    	    apool :initial-delay 360000)
      	    	    		    (proxy-super write c)))))]
        (doseq [stream [#'*out* #'*err*]]
          (alter-var-root stream (constantly wrapt)))
        (try (lazybot/start-server (:servers-port config 8080))
             (lazybot/initiate-mongo)
             (irc/start-bots (:servers config))
          (catch Exception e (do 
          		  (.printStackTrace e) 
          		  (aa/after 360000 #(do
          		  		  (.exec (Runtime/getRuntime) reboot-string)
          		  		  (.println System/out "RECONNECTING")
          		  		  (System/exit 0))
          		  	  apool))))))))

