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