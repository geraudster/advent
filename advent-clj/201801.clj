(require '[clojure.string :as str])
(require '[clojure.data :as data])
(require '[clojure.core.reducers :as r])

;;(let [
;;      input (flatten (repeat (str/split (slurp "/home/geraud/Downloads/input") #"\n")))
;;      [_ & previous] input
;;      ]
;;  (take 10 (r/fold + (r/map int (r/map #(str/replace-first %1 "+" "") input)))))

;;; Day 1
(defn get-cumul []
  (let [
        input (flatten (repeat (str/split (slurp "/home/geraud/Downloads/input") #"\n")))
        int-input (map (fn [x] (Integer/parseInt (str/replace-first x "+" ""))) input)
        ]
    (reductions
     (fn [acc x]
       (let [cumul (+ (last (:cumuls acc)) x)
             freqs (:freqs acc)
             cumul-key (keyword (str cumul))]
         {:cumuls (conj (:cumuls acc) cumul)
          :freqs (update freqs cumul-key (fn [n] (if (nil? n) 1 (inc n))))}))
     {:cumuls [0] :freqs {:0 1}}
     int-input)))

(def result
  (->> 
   (get-cumul)
   (drop-while (fn [stat] (every? #(<= % 1) (vals (:freqs stat)))))
   (first)
   (:freqs)))

(keep #(when (> (val %) 1)
         (key %)) result)
result

{:a 1}
(update (:freq {:cumul [0] :freq {:0 1 :10 123}}) :0 (fn [n] (if (nil? n) 0 (inc n))))

{:cumul [0] :freq {:0 1}}
(:freq {:cumul [0] :freq {:0 1}})

;;; Day 2
(defn get-checksum []
    (->>
     (str/split (slurp "input2") #"\n")
     (map (fn [id] (filter #(> % 1) (keys (clojure.set/map-invert (frequencies id))))))
     (flatten)
     (frequencies)
     (vals)
     (reduce *)
     ))

(get-checksum)

(defn get-similar []
  (let [ m (map (fn [id]
                  (str/split id #""))
                (str/split (slurp "input2") #"\n")) ]
    (for [a m
          b m]
      (->>
       (map = a b)
       (reduce (fn [acc x] (if x acc (inc acc))) ,,, 0)
       (assoc {:a a :b b} :diff))
     )))


(let [ match (->> (get-similar)
                  (drop-while (fn [x] (not= 1 (:diff x))))
                  (first)) ]
  (->>
   (last (data/diff (:a match) (:b match)))
   (filter some?)
   (str/join)
   ))


(->>
 (map = (str/split "fghiaj" #"") (str/split "fguibj" #""))
 (reduce (fn [acc x] (if x (inc acc) acc)) ,,, 0)
 (assoc {} :diff))

;;; Day 3
(defn get-common-squares []
  (let [input (str/split (slurp "input3") #"\n")
        parsed (map
                (fn [x]
                  (let [[id x y width height]
                        (->> x
                             (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                             (rest)
                             (map #(Integer/parseInt %)))]
                    {:id id :x x :y y :width width :height height}))
                input)]
    (mapcat (fn [claim]
              (let [squares
                    (for [x (range (:x claim) (+ (:x claim) (:width claim)))
                          y (range (:y claim) (+ (:y claim) (:height claim)))]
                      {:x x :y y})]
                squares))
         parsed
         )))

(->>
 (get-common-squares)
 (frequencies)
 (filter (comp #(> % 1) val))
 (count)
)

(defn get-squares []
  (let [input (str/split (slurp "input3") #"\n")
        parsed (map
                (fn [x]
                  (let [[id x y width height]
                        (->> x
                             (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                             (rest)
                             (map #(Integer/parseInt %)))]
                    {:id id :x x :y y :width width :height height}))
                input)]
    (map (fn [claim]
           (let [squares
                 (for [x (range (:x claim) (+ (:x claim) (:width claim)))
                       y (range (:y claim) (+ (:y claim) (:height claim)))]
                   {:x x :y y})]
             {:claim (:id claim) :squares squares}))
         parsed
         )))

(defn get-non-overlaping-id []
  (let [squares (get-squares)
        non-overlapped-squares (->>
                               (get-common-squares)
                               (frequencies)
                               (filter (comp #(= % 1) val))
                               (keys)
                               )]
    ;;    (filter (fn [square] (every? (map #(contains?  (keys square)))))
    ;;    non-overlapped-squares
    (->>
     squares
     (filter
      (fn [square]
        (every? (fn [coord] (some #{coord} non-overlapped-squares)) (:squares square))))
            
)))

(get-squares)
(get-non-overlaping-id)

(->>
 [{:x 0 :y 1} {:x 12 :y 13}]
 (every? (fn [coord] (some #{coord} [{:x 0 :y 1} {:x 12 :y 13}]))))

(some #{{:x 0 :y 1}} [{:x 0 :y 1} {:x 1 :y 2}])

;;; Day 7

(defn export-graph []
  (let [input (str/split (slurp "input7") #"\n")]
    (->> input
         (map #(str/replace % #"Step (\w) must be finished before step (\w) can begin." "$1,$2,BEFORE"))
    )))

(spit "graph7.csv" (str/join "\n" (export-graph)))


;; // Contraint creation
;; CREATE CONSTRAINT ON (step:Step) ASSERT step.id IS UNIQUE

;; // Import data
;; LOAD CSV FROM "file:///graph7.csv" as line
;; MERGE (a:Step { id: line[0]})
;; MERGE (b:Step { id: line[1]})
;; CREATE (a) -[:BEFORE]-> (b);

;; MATCH p=((a)-[r:BEFORE*1..100]->(b))
;; WHERE NOT ()-[:BEFORE]->(a)
;; AND NOT (b)-[:BEFORE]->()
;; RETURN extract(n in nodes(p) | n.id) as labels

;;; Day 5

(let
    [
     alphabet (str/split "abcdefghijklmnopqrstuvwxyz" #"")
     uppercase (map str/upper-case alphabet)
     lowercase alphabet
     all (concat (map vector lowercase uppercase)
                 (map vector uppercase lowercase))
     patterns (str/join "|" (map str/join all))]
  (loop [text (slurp "input5")]
    (if (not (re-find (re-pattern patterns) text))
      text
    (recur (str/replace text (re-pattern patterns) ""))))
)
      
