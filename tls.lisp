(defun lat? (l)
  (cond
    ((null l) t)
    ((atom (car l)) (lat? (cdr l)))
    (t nil)))

(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t (or (eq (car lat) a)
        (member? a (cdr lat))))))

(defun rember (a lat)
  (cond
    ((null lat) '())
    ((eq (car lat) a) (cdr lat))
    (t (cons (car lat)
             (rember a (cdr lat))))))

(defun firsts (l)
  (cond
    ((null l) '())
    (t (cons (car (car l))
             (firsts (cdr l))))))

(defun seconds (l)
  (cond
    ((null l) '())
    (t (cons (car (cdr (car l)))
             (firsts (cdr l))))))

(defun insertR (new old lat)
  (cond
    ((null lat) '())
    ((eq (car lat) old) (cons old (cons new (cdr lat))))
    (t (cons (car lat)
             (insertR new old (cdr lat))))))

(defun insertL (new old lat)
  (cond
    ((null lat) '())
    ((eq (car lat) old) (cons new lat))
    (t (cons (car lat)
             (insertL new old (cdr lat))))))

(defun subst (new old lat)
  (cond
    ((null lat) '())
    ((eq (car lat) old) (cons new (cdr lat)))
    (t (cons (car lat)
             (subst new old (cdr lat))))))

; end of LISPer, bonus practice
(defun subst2 (new o1 o2 lat)
  (cond
    ((null lat) '())
    ((or (eq (car lat) o1)
         (eq (car lat) o2)) (cons new (cdr lat)))
    (t (cons (car lat)
             (subst new old (cdr lat))))))

(defun multirember (a lat)
  (cond
    ((null lat) '())
    ((eq (car lat) a) (multirember a (cdr lat)))
    (t (cons (car lat)
             (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
  (cond
    ((null lat) '())
    ((eq (car lat) old) (cons old
                              (cons new
                                    (multiinsertR new old (cdr lat)))))
    (t (cons (car lat)
             (multiinsertR new old (cdr lat))))))

(defun multisubst (new old lat)
  (cond
    ((null lat) '())
    ((eq (car lat) old) (cons new (multisubst new old (cdr lat))))
    (t (cons (car lat)
             (multisubst new old (cdr lat))))))

; purely academic exercise as these will fail badly
; if m is negative
(defun plus (n m)
  (cond
    ((zerop m) n)
    (t (plus (1+ n) (1- m)))))

(defun minus (n m)
  (cond
    ((zerop m) n)
    (t (minus (1- n) (1- m)))))

(defun x (n m)
  (cond
    ((zerop m) m)
    (t (+ n (x n (1- m))))))

; this is clean though
(defun addtup (tup)
  (cond
    ((null tup) 0)
    (t (+ (car tup) (addtup (cdr tup))))))

(defun tup+ (tup1 tup2)
  (cond
    ((and (null tup1) (null tup2)) '())
    ((null tup1) tup2)
    ((null tup2) tup1)
    (t (cons (+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2))))))
