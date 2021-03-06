(ns lazybot.registry
  (:require [useful.fn :refer [fix to-fix]]
            [lazybot.utilities :refer [on-thread verify validator]]
            [useful.fn :refer [!]]
            [clojail.core :refer[thunk-timeout]]
            [clojure.string :refer [join]]
            [irclj.core :as ircb]
            [somnium.congomongo :as mongo])
  (:import java.util.concurrent.TimeoutException))

;; ## Hook handling
(defn pull-hooks [bot hook-key]
  (map :fn
       (hook-key
        (apply merge-with concat
               (map :hooks
                    (vals (:modules @bot)))))))

(defn nil-comp [com bot channel s action? & fns]
  (reduce #(when %1
             (%2 com bot channel %1 action?))
          s fns))

(defn call-message-hooks [com bot channel s action?]
  (apply nil-comp com bot channel s action? (pull-hooks bot :on-send-message)))

;; ## Command searching
(defn find-command [modules command]
  (some (validator #((:triggers %) command))
        (apply concat (map :commands (vals modules)))))

(defn find-docs [bot command]
  (:docs (find-command (:modules @bot) command)))

(defn respond [{:keys [command bot]}]
  (let [cmd (find-command (:modules @bot) command)]
    (or (:fn cmd) (constantly nil))))

;; ## Command parsing

(defn full-prepend [config s]
  ((:prepends config) (str s)))

(defn m-starts-with [m s]
  (some #(.startsWith m %) s))

(defn cmd-map [cmd args]
  {:command cmd, :args args, :raw-args (join " " args)})

(defn split-args [config s query?]
  (let [[fst & args] (.split s " ")]
    (cond (full-prepend config fst)         (cmd-map (first args) (rest args))
          (full-prepend config (first fst)) (cmd-map (-> fst rest join) args)
          query?                            (cmd-map fst args))))

(defn is-command?
  "Tests whether or not a message begins with a prepend."
  [message prepends]
  (m-starts-with message prepends))

;; ## Command handling

;; This is what you should use for sending messages.
;; TODO: Document
(defn send-message [{:keys [bot com channel] :as irc} s
                    & {:keys [action? notice?]}]
  (let [result (call-message-hooks com bot channel s action?)
        writer (cond
                action? ircb/ctcp
                notice? ircb/notice
                :else ircb/message)]
    (writer com channel result)))

(defn ignore-message? [{:keys [nick bot com]}]
  (-> bot
      (get-in [:config (:server com) :user-blacklist])
      (#(some (fn [re] (re-find (re-pattern (str "(?i)" re)) nick)) %))))

(defn try-handle
  "attempt to handle input from an IRC channel"
  [{:keys [nick bot-nick channel bot message event query?] :as state}]
  {:pre [(= nick (:nick event))]}
  (when-not (ignore-message? state)
    (let [{:keys [config pending-ops]} @bot
          {:keys [max-operations prepends]} config
          respond? (or (is-command? message prepends)
                       query?)
          overtaxed? (>= pending-ops max-operations)]
      (when respond?
        (if overtaxed?
          (send-message state "Too much is happening at once. Wait until other operations cease.")
          (try
            (dosync (alter bot update-in [:pending-ops] inc))
            (let [state (into state (split-args config message query?))]
              (thunk-timeout #((respond state) state)
                             30 :sec))
            (catch TimeoutException _
              (send-message state "Execution timed out."))
            (catch Exception e (.printStackTrace e))
            (finally
              (dosync (alter bot update-in [:pending-ops] dec)))))))))

;; ## Plugin DSL
(defn merge-with-conj [& args]
  (apply merge-with #(if (vector? %) (conj % %2) (conj [] % %2)) args))

(defn parse-fns [body]
  (apply merge-with-conj
         (for [[one & [two three four :as args]] body]
           {one
            (case
             one
             :cmd {:docs two
                   :triggers three
                   :fn four}
             :hook {two {:fn three}}
             :indexes (vec args)
             two)})))

(defn if-seq-error [fn-type possible-seq]
  (if (and (not (fn? possible-seq)) (seq possible-seq))
    (throw (Exception. (str "Only one " fn-type " function allowed.")))
    possible-seq))

;; Wrap isolated objects with a vector
(def make-vector (to-fix (! vector?) vector))

;; This is the meat -- our plugin DSL.
(defmacro defplugin [& body]
  (let [{:keys [cmd hook cleanup init indexes routes]} (parse-fns body)
        scmd (if (map? cmd) [cmd] cmd)]
    `(let [pns# *ns*
           m-name# (keyword (last (.split (str pns#) "\\.")))]
       (defn ~'load-this-plugin [com# bot#]
         (when ~init ((if-seq-error "init" ~init) com# bot#))
         (doseq [idx# ~indexes]
           (apply mongo/add-index! m-name# idx#))
         (dosync
          (alter bot# assoc-in [:modules m-name#]
                 {:commands ~scmd
                  :hooks (into {}
                               (for [[k# v#] (apply merge-with-conj
                                                    (make-vector ~hook))]
                                 [k# (make-vector v#)]))
                  :cleanup (if-seq-error "cleanup" ~cleanup)
                  :routes ~routes}))))))
