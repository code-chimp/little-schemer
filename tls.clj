(defn atom? [a]
  (not (seq? a)))

(defn null? [a]
  (or (= [] a)
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
    (null? lat) []
    (= (first lat) a) (rest lat)
    :else (cons (first lat)
                (rember a (rest lat)))))

(defn firsts [l]
  (cond
    (null? l) []
    :else (cons (first (first l))
                (firsts (rest l)))))

(defn insertR [n o lat]
  (cond
    (null? lat) []
    (= (first lat) o) (cons o (cons n (rest lat)))
    :else (cons (first lat)
                (insertR n o (rest lat)))))

(defn insertL [n o lat]
  (cond
    (null? lat) []
    (= (first lat) o) (cons n lat)
    :else (cons (first lat)
                (insertL n o (rest lat)))))

(defn subst [n o lat]
  (cond
    (null? lat) []
    (= (first lat) o) (cons n (rest lat))
    :else (cons (first lat)
                (subst n o (rest lat)))))

; end of LISPer, bonus practice
(defn subst2 [n o1 o2 lat]
  (cond
    (null? lat) []
    (or (= (first lat) o1)
        (= (first lat) o2)) (cons n (rest lat))
    :else (cons (first lat)
                (subst2 n o1 o2 (rest lat)))))

(defn multirember [a lat]
  (cond
    (null? lat) []
    (= (first lat) a) (multirember a (rest lat))
    :else (cons (first lat)
                (multirember a (rest lat)))))

(defn multiinsertR [n o lat]
  (cond
    (null? lat) []
    (= (first lat) o) (cons o (cons n
                                    (multiinsertR n o (rest lat))))
    :else (cons (first lat)
                (multiinsertR n o (rest lat)))))

(defn multisubst [n o lat]
  (cond
    (null? lat) []
    (= (first lat) o) (cons n
                            (multisubst n o (rest lat)))
    :else (cons (first lat)
                (multisubst n o (rest lat)))))

(defn add1 [x]
  (+ x 1))

(defn sub1 [x]
  (- x 1))

; purely academic exercise as the following would
; fail badly if m is negative
(defn plus [n m]
  (cond
    (zero? m) n
    :else (plus (add1 n) (sub1 m))))

(defn minus [n m]
  (cond
    (zero? m) n
    :else (minus (sub1 n) (sub1 m))))

(defn x [n m]
  (cond
    (zero? m) m
    :else (+ n (x n (sub1 m)))))

(defn ↑ [n m]
  (cond
    (zero? m) 1
    :else (* n (↑ n (sub1 m)))))

; this is clean though
(defn addtup [tup]
  (cond
    (null? tup) 0
    :else (+ (first tup) (addtup (rest tup)))))

(defn tup+ [tup1 tup2]
  (cond
    (null? tup1) tup2
    (null? tup2) tup1
    :else (cons (+ (first tup1) (first tup2))
                (tup+ (rest tup1) (rest tup2)))))

(defn myDiv [n m]
  (cond
    (< n m) 0
    :else (add1 (myDiv (- n m) m))))

(defn length [lat]
  (cond
    (null? lat) 0
    :else (add1 (length (rest lat)))))

(defn pick [n lat]
  (cond
    (null? lat) nil
    (zero? (sub1 n)) (first lat)
    :else (pick (sub1 n) (rest lat))))

(defn rempick [n lat]
  (cond
    (null? lat) nil
    (= n 1) (rest lat)
    :else (cons (first lat)
                (rempick (sub1 n) (rest lat)))))

(defn no-nums [lat]
  (cond
    (null? lat) []
    (number? (first lat)) (no-nums (rest lat))
    :else (cons (first lat)
                (no-nums (rest lat)))))

(defn all-nums [lat]
  (cond
    (null? lat) []
    (not (number? (first lat))) (all-nums (rest lat))
    :else (cons (first lat)
                (all-nums (rest lat)))))

(defn occur [a lat]
  (cond
    (null? lat) 0
    (= (first lat) a) (add1 (occur a (rest lat)))
    :else (occur a (rest lat))))

(defn rember* [a l]
  (cond
    (null? l) []
    (atom? (first l))
      (cond
        (= (first l) a) (rember* a (rest l))
        :else (cons (first l)
                    (rember* a (rest l))))
    :else (cons (rember* a (first l))
                (rember* a (rest l)))))

