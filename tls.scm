(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define member2?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? old (car lat)) (cons old (cons new (cdr lat))))
       (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? old (car lat)) (cons new lat))
       (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? old (car lat)) (cons new (cdr lat)))
       (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
     (cond
       ((null? lat) '())
       ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
       (else (cons (car lat) (subst new old (cdr lat)))))))
