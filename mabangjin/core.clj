;;마방진
(defn set-result [number]
	(vec (repeat number (vec (repeat number nil)))))

(defn abs [n] (max n (- n)))

(defn check-range [x n]
	(if (and (<= 0 x) (< x n)) x (- n (abs x))))

(defn next-place [a b n r]
	(let [x (check-range (+ a 1) n) y (check-range (- b 1) n)]
		(if (nil? (nth (nth r y) x))
			[x y]
			[a (check-range (+ b 1) n)])))

(defn magic-square [number]
	(if (odd? number)
		(let [values (range 1 (+ (* number number) 1)) result (set-result number)]
		(loop [[x y] [1 0] n 0 r result]
			(if (= n (count values))
				r
				(recur (next-place x y (count result) r) (+ n 1) (assoc r y (assoc (nth r y) x (nth values n)))))
		)) "odddddd!"))