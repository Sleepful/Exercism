(ns bird-watcher)

(def last-week [0 2 5 3 7 8 4])

; (get last-week (- (count last-week) 1))

(defn today [birds] (peek birds))

(defn inc-bird [birds]
  (update  birds  (- (count birds) 1) inc))

(defn day-without-birds? [birds] (some? (some zero? birds)))

(defn n-days-count [birds n]
  (let [birds (take n birds)]
    (reduce + birds)))

(defn busy-days [birds]
  (count
   (filter (fn [bird] (<= 5 bird))  birds)))

(defn odd-week? [birds] (= [1 0 1 0 1 0 1] birds))

