(defn atom? [a]
  (not (seq? a)))

(defn null? [a]
  (or (= '() a)
      (nil? a)))

(defn abs [x]
  (cond
    (< x 0) (* x -1)
    :else x))

(defn add1 [x]
  (+ x 1))

(defn sub1 [x]
  (- x 1))
