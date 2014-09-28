(defun lat? (l)
  (cond
    ((null l) t)
    ((atom (car l)) (lat? (cdr l)))
    (t nil)))

(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t (or (eq a (car lat))
        (member? a (cdr lat))))))

(defun rember (a lat)
  (cond
    ((null lat) '())
    ((eq a (car lat)) (cdr lat))
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
    ((eq old (car lat)) (cons old (cons new (cdr lat))))
    (t (cons (car lat) (insertR new old (cdr lat))))))

(defun insertL (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat)) (cons new lat))
    (t (cons (car lat) (insertL new old (cdr lat))))))

; end of LISPer, bonus practice
(defun subst (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat)) (cons new (cdr lat)))
    (t (cons (car lat) (subst new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  (cond
    ((null lat) '())
    ((or (eq o1 (car lat))
         (eq o2 (car lat))) (cons new (cdr lat)))
    (t (cons (car lat) (subst new old (cdr lat))))))
