(defn atom? [a]
  (not (seq? a)))

(defn null? [a]
  (or (= '() a)
      (nil? a)))

(defn lat? [l]
  (cond
    (null? l) true
    (atom? (first l)) (lat? (rest l))
    :else false))

(defn member? [a lat]
  (cond
    (null? lat) false
    :else (or (= (first lat) a)
              (member? a (rest lat)))))

(defn rember [a lat]
  (cond
    (null? lat) '()
    (= (first lat) a) (rest lat)
    :else (cons (first lat)
                (rember a (rest lat)))))

(defn firsts [l]
  (cond
    (null? l) '()
    :else (cons (first (first l))
                (firsts (rest l)))))

; insertR
(defn insertR [n o lat]
  (cond
    (null? lat) '()
    (= (first lat) n) (cons o (cons n (rest lat)))
    :else (cons (first lat) (insertR n o (rest lat)))))

(defn abs [x]
  (cond
    (< x 0) (* x -1)
    :else x))

(defn add1 [x]
  (+ x 1))

(defn sub1 [x]
  (- x 1))
