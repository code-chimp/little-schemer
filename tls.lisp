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

; end of LISPer, bonus practice
(defun subst (new old lat)
  (cond
    ((null lat) '())
    ((eq (car lat) old) (cons new (cdr lat)))
    (t (cons (car lat)
             (subst new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  (cond
    ((null lat) '())
    ((or (eq (car lat) o1)
         (eq (car lat) o2)) (cons new (cdr lat)))
    (t (cons (car lat)
             (subst new old (cdr lat))))))
