(ns pi.core
  (:use clojure.contrib.generic.math-functions))

(defn gauss-legendre [places]
  (let [error (BigDecimal. (pow 10 (- places)))]
    (with-precision (+ places 1) :rounding HALF_DOWN
		    (loop [a 1.0M
			   b (/ 1.0M  (BigDecimal. (sqrt 2)))
			   t 0.25M
			   p 1.0M]
		      (if (< error (- a b))
			(do
			  (prn "a: " a "b: " b "t: " t "p: " p)
			  (let [anplusone (/ (+ a b) 2.0M)]
			    (recur anplusone
				   (BigDecimal. (sqrt (* a b)))
				   (- t (* p (sqr (- a anplusone))))
				   (* 2.0M p))))
			(do (/ (sqr (+ a b)) (* 4.0M t))))))))

(defn nth-decimal [number decimal]
   (int (mod (* number (pow 10 decimal)) 10)))

(defn pi-digits
  []
  (map #(nth-decimal (gauss-legendre %) %) (range)))
