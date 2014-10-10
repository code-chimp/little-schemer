(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
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
       ((eq? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat)
                   (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new lat))
       (else (cons (car lat)
                   (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new (cdr lat)))
       (else (cons (car lat)
                   (subst new old (cdr lat)))))))

; end of LISPer, bonus practice
(define subst2
  (lambda (new o1 o2 lat)
     (cond
       ((null? lat) '())
       ((or (eq? (car lat) o1)
            (eq? (car lat) o2)) (cons new (cdr lat)))
       (else (cons (car lat)
                   (subst new old (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons old
                                  (cons new
                                        (multiinsertR new old (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertR new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new
                                  (multisubst new old (cdr lat))))
       (else (cons (car lat)
                   (multisubst new old (cdr lat)))))))

; guile has it, mit doesn't
(define 1-
  (lambda (x)
    (- x 1)))

; purely academic exercise as these will fail badly if
; m is negative
(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (plus (1+ n) (1- m))))))

(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (minus (1- n) (1- m))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) m)
      (else (+ n (x n (1- m)))))))

; mit-scheme don't cotton to digraphs
(define myPow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (myPow n (1- m)))))))

; this is clean though
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define myDiv
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (1+ (myDiv (- n m) m))))))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (1+ (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) '())
      ((zero? (1- n)) (car lat))
      (else (pick (1- n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) '())
      ((= n 1) (cdr lat))
      (else (cons (car lat)
                  (rempick (1- n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((not (number? (car lat))) (all-nums (cdr lat)))
      (else (cons (car lat)
                  (all-nums (cdr lat)))))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (1+ (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

