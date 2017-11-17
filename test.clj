(defn gcd [x y]
	(if (> x y)
		(recur y x)
		(if (= 0 (mod y x))
			x
			(recur x (mod y x)))))

(defn lcm [& args]
	(letfn [(gcd [x y]
				(if (> x y)
					(recur y x)
					(if (= 0 (mod y x))
						x
						(recur x (mod y x)))))
			(llccmm [a b]
				(/ (* a b) (gcd a b)))]
		(reduce llccmm args))
	)

(defn pascal [l]
	(letfn [(hh [l]
			(conj
			(vec (conj (map (fn [x y]
						(+ x y))
						(into [] l) (rest (into [] l))) 1)) 1)
			)]
		(last (take l (iterate hh [1]))))
	)

(defn sss [a]
(letfn [(asdf [l]
	(print l)
	(cond
		(= clojure.lang.PersistentVector (class l)) (if (not= (count l) 3) false (map asdf l))
		(= clojure.lang.PersistentList (class l)) (if (not= (count l) 3) false (map asdf l))
        :else	(if (or (= l ()) (= l false)) false true))
	)]
	(if (= (count a) 3) (every? identity (asdf a)) false)
))

(defn qwer [r l]
	(letfn [(asdf [l]
			(cond
				(= clojure.lang.PersistentVector (class l)) (if (not= (count l) 3) false true)
				(= clojure.lang.PersistentList (class l)) (if (not= (count l) 3) false true)
		        :else	(if (= l ()) false true)))])
	(when (= (asdf 1) '()) false)
	(and (asdf l) r)
)

﻿(defn istree? [root]
  (print root)
  (or (nil? root)
      (and (sequential? root)
           (= 3 (count root))
           (every? istree? (rest root)))))


(defn aaa [x y z]
	(x z y))





(asfd 2 [1 2 3 4 5])

(
(fn [a b] (take (count b)
((fn [x y]
	(loop [c x out (cycle y)]
		(if (= c 0) out
			(recur (dec c) (rest out))
			))) a b))) 2 [1 2 3 4 5])

(aaa 2 [1 2 3 4 5])

(take)

(= ((fn [a b] (take (count b)
((fn [x y]
	(loop [c x out (cycle y)]
		(if (= c 0) out
			(recur (dec c) (rest out))
			))) a b))) 2 [1 2 3 4 5]) '(3 4 5 1 2))


(partition (/ (count %1) %2) (apply interleave (partition %2 %1)))


#(map second (group-by type %))


(fn [l]
	(every? true? (apply list (map (fn [x] if (= 0 (mod l x)) false true) (drop 2 (range l)))))
	)


(defn aaa [abc]
	(letfn [(prime? [l]
				(every? true? 
					(apply list 
						(map 
							(fn [x] 
								(if (= 0 (mod l x)) false true)
								)
								(drop 2 (range l))
								)
						)
					)
			)]
	(take abc (remove nil? (drop 2 (map (fn [c]
		(when (prime? c) c)) (range)))))
	)
	)


(defn anagram [keyw l]
	(loop [out #{} in l]
		(if (empty? in) out
			(if (= (set keyw) (set (first in)))
				(recur (conj out (first in)) (rest in))
				(recur out (rest in))
			))))

(defn main [l]
	(letfn [(anagram [keyw l]
				(loop [out #{} in l]
					(if (empty? in) out
						(if (= (set keyw) (set (first in)))
							(recur (conj out (first in)) (rest in))
							(recur out (rest in))
						))))]
	
	(loop [out #{} in l]
		(if (empty? in) (set (filter #(not= (count %) 1) out))
		(recur (conj out (anagram (first in) l)) (rest in))
		))
	))

scones
meetme
------
egsgqw

(def alpha2 [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z])
(defn encode [k w]
	(letfn [(hash [w k]
				(let [int_word (- (int w) 97) int_key (- (int k) 97)]
				(nth (drop int_word (cycle alpha2)) int_key)
				)
			)
			]
			(apply str (map hash w (cycle k)))		
		)
	)

(defn decode [k r]
	(letfn [(getindex [r k]
				(let [int_key (- (int k) 97)]
					(nth alpha2
						(.indexOf (take (count alpha2) (drop int_key (cycle alpha2))) r))
					)
			)]
		(apply str (map getindex r (cycle k)))
		)
	)

(def ra (range 100000 1000000))

(defn wonderland-number []
	(letfn [(find [x]
				(when (= (set (str x)) (set (str (* 2 x))))
					(when (= (set (str x)) (set (str (* 3 x))))
						(when (= (set (str x)) (set (str (* 4 x))))
							(when (= (set (str x)) (set (str (* 5 x))))
								(when (= (set (str x)) (set (str (* 6 x))))
									x
								)))))
				)]
	(remove nil? (map find ra))
	)
	)




(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
	(for [suit suits
			rank ranks]
		[suit rank]))

(defn start []
	(let [a (partition-all (/ (count cards) 2) (shuffle cards))]
		(play-game (first a) (last a))
		)
	)

(defn get-score[[suit rank]]
	(+ (* 100 (.indexOf ranks rank))
		(.indexOf suits suit))
	)

(defn play-round [player1-cards player2-cards]
	(let [p1 (first player1-cards) p2 (first player2-cards)]
		(if (> (get-score p1) (get-score p2))
				;;play1 wins the round
				[(concat (rest player1-cards) [p1 p2]) (rest player2-cards)]
				[(rest player1-cards) (concat (rest player2-cards) [p1 p2])]
				)
		)
	)

(defn play-game [player1-cards player2-cards]
;; if no card lose
	(loop [[p1 p2] (play-round player1-cards player2-cards)]
		(println (str (first p1) (count p1) " " (first p2) (count p2)))		
		(when (empty? p1) "player2 wins")
		(when (empty? p2) "player1 wins")
		(recur (play-round p1 p2))
	))


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

;chatting


(defn chat-server )









