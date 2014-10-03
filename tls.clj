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

(defn insertR [n o lat]
  (cond
    (null? lat) '()
    (= (first lat) o) (cons o (cons n (rest lat)))
    :else (cons (first lat)
                (insertR n o (rest lat)))))

(defn insertL [n o lat]
  (cond
    (null? lat) '()
    (= (first lat) o) (cons n lat)
    :else (cons (first lat)
                (insertL n o (rest lat)))))

(defn subst [n o lat]
  (cond
    (null? lat) '()
    (= (first lat) o) (cons n (rest lat))
    :else (cons (first lat)
                (subst n o (rest lat)))))

; end of LISPer, bonus practice
(defn subst2 [n o1 o2 lat]
  (cond
    (null? lat) '()
    (or (= (first lat) o1)
        (= (first lat) o2)) (cons n (rest lat))
    :else (cons (first lat)
                (subst2 n o1 o2 (rest lat)))))

(defn multirember [a lat]
  (cond
    (null? lat) '()
    (= (first lat) a) (multirember a (rest lat))
    :else (cons (first lat)
                (multirember a (rest lat)))))

(defn add1 [x]
  (+ x 1))

(defn sub1 [x]
  (- x 1))
