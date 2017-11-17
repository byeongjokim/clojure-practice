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