#lang racket

(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (λ (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define rember
  (λ (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))

(define 1+
  (λ (x)
    (+ x 1)))

(define 1-
  (λ (x)
    (- x 1)))
