(ns lazybot.plugins.markov
  "Credit goes to Martin Janiczek, https://github.com/Janiczek/markov . EPL
  licensed like Clojure and the rest of Lazybot."
  (:use  [lazybot registry]
         [lazybot.utilities :only [prefix]]
  	 [clojure.string :only [join split]]
  	 [somnium.congomongo :only [fetch fetch-one insert! destroy!]]))

(defn prepare-string [string]
  (split (clojure.string/lower-case string) #"(\s|[\.,\!\?;\:\"\(\)\[\}\{\}\/\\])+"))
(defn create-counts [order coll]
  "Computes how many times did each 'next state' come from a 'previous state'.
  Order must be < (count coll).
  The result type is {previous_state {next_state count}}."
  (let [past    (butlast (map vec (partition order 1 coll)))
        present (drop order coll)
        zipped  (map vector past present)
        sorted  (sort zipped)
        grouped (group-by first sorted)
        seconds (map (fn [[k pairs]] [k (map second pairs)]) (seq grouped))
        freqs   (map (fn [[k secs]]  [k (frequencies secs)]) seconds)]
    (into {} freqs)))

(defn- create-totals [count-map]
  "Computes the number of occurences of each state.
   The result type is {state count}."
  (let [totals (map (fn [[k counts]] [k (apply + (vals counts))])
                    (seq count-map))]
    (into {} totals)))

(defn- create-probs [count-map]
  "Computes the probabilities of each of the transitions.
   That's done by normalizing their counts into interval <0,1>.
   The result type is {previous_state {next_state probability}}."
  (let [totals (create-totals count-map)
        probs  (map (fn [[ke counts]]
                      (let [the-total (get totals ke)]
                        [ke (into {} (map (fn [[k c]] [k (/ c the-total)])
                                           (seq counts)))]))
                    (seq count-map))]
    (into {} probs)))

(defn- take-from-probs [probs]
  ; probs = {:a 1/2 :b 1/3 :c 1/6}
  ; pseq  = [[:a 1/2] [:b 1/3] [:c 1/6]]
  ; added = [[:a 1/2] [:b 5/6] [:c 1]]
  "Given a map of probabilities that add up to 1, takes one randomly."
  (let [rval  (rand)
        pseq  (reverse (sort-by second probs)) ; sorts and makes a seq, yay!
        added (rest (reduce #(conj %1 [(first %2) (+ (second (last %1))
                                                     (second %2))])
                            [[:whatever 0]]
                            pseq))]
    (first (first (drop-while #(>= rval (second %)) added)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn build-from-coll
  "Computes probabilities (a transition matrix) of given order from collection."
  ([coll] (build-from-coll 1 coll))
  ([order coll]
   (if (and (sequential? coll)
            (< order (count coll)))
     (create-probs (create-counts order coll)))))

(defn build-from-string
  "Converts string to collection and computes its transition matrix."
  ([string] (build-from-string 1 string))
  ([order string] (build-from-coll order (prepare-string string))))

#_(defn build-from-file
  "Reads a file into a collection and computes its transition matrix."
  ([filepath] (build-from-file 1 filepath))
  ([order filepath] (build-from-coll order (prepare-file filepath))))

(defn generate-walk
  "Generates a random 'walk' given the transition matrix and (opt.) starting values.
  If n = higher order transition matrix > 1, needs at least n starting values!
  Can stop when it gets to a state it wasn't trained on!
  For example, ABACAD -> if we ever get to D, we end."
  ([probs] (generate-walk (first (rand-nth (seq probs)))
                              probs))
  ([start probs] 
   (if (not (sequential? start))
     (generate-walk [start] probs)
     (let [order (count (first (first (seq probs))))]
       (if (< (count start) order)
         nil
         (letfn [(lazy-walk [last-state]
                   (let [next-state (take-from-probs (get probs last-state))
                         next-args  (conj (rest last-state) next-state)]
                     (if (or (nil? next-state) (= "艚" next-state) (and (seq next-state) (some #{"艚"} next-state)))
                       nil ; cons _ nil = (_)
                       (cons next-state (lazy-seq (lazy-walk next-args))))))]
           (lazy-cat start (lazy-walk (take-last order start)))))))))
(defn get-all-messages
	[server]
	(mapv :message (remove #(= (:message % ) "I love you, creepybot!")
  		  (fetch :last :limit 2000 :sort {:time -1} :where {:server server}))))
(defn get-all-messages-by
	[server nick]
	(mapv :message (remove #(= (:message % ) "I love you, creepybot!")
  		  (fetch :last :limit 2000 :sort {:time -1} :where {:server server :nick nick}))))
(def all-quotes (atom {}))
(def enabled (atom #{}))
(defplugin
  (:hook :privmsg
  	  (fn [{:keys [com bot nick channel message] :as com-m}] (if
  	  		  (or (> (rand-int 100) 2)
  	  		      (not (contains? @enabled channel))
  	  		      (re-find #"^[`!]" message))
         		 nil
         		 (send-message com-m (clojure.string/capitalize (clojure.string/replace (join " " (take 30 (generate-walk @all-quotes)))
         		 		 #"$" (rand-nth ["." "." "." "" "" "!" "?" "..."]))))
         		 )))
  (:cmd
   "Get a single mashup of various quotes people `grab-bed."
   #{"mashup"}
   (fn [{:keys [bot nick com channel args] :as com-m}]
   	   (do (reset! all-quotes (build-from-string 1 (apply str (map #(str % " 艚 ") (get-all-messages (:server @com))))))
               (send-message com-m (clojure.string/capitalize (clojure.string/replace (join " " (take 30 (generate-walk @all-quotes)))
         		 		 #"$" (rand-nth ["." "." "." "" "" "!" "?" "..."])))))))
  (:cmd
   "Get a single mashup of various quotes by a single person."
   #{"mimic"}
   (fn [{:keys [bot nick com channel args] :as com-m}]
   	   (do
               (send-message com-m (clojure.string/capitalize (clojure.string/replace (join " " (take 30 (generate-walk
               						       (build-from-string 1 (apply str (map #(str % " 艚 ") (get-all-messages-by (:server @com) (first args))))))))
         		 		 #"$" (rand-nth ["." "." "." "" "" "!" "?" "..."])))))))
  (:cmd
   "Enable markov chaining and random blurting out of quote mashups. Disable with !markoff"
   #{"markov"}
   (fn [{:keys [bot nick com channel args] :as com-m}]
     (do
       (reset! all-quotes (build-from-string 1 (apply str (map #(str % " 艚 ") (get-all-messages (:server @com))))))
       (when (not (contains? @enabled channel))
       	       (swap! enabled conj channel)
       	       (send-message com-m "Markov chaining enabled. I will blurt out a mashup of quotes in response to 3% of messages until disabled with `markoff .")))))
  (:cmd
   "Disable markov chaining and random blurting out of quote mashups. Enable with !markov"
   #{"markoff"}
   (fn [{:keys [bot nick channel args] :as com-m}]
     (do
       (when (contains? @enabled channel) (swap! enabled disj channel))      
       (send-message com-m "Markov chaining disabled. This can be re-enabled with `markov .")))))
