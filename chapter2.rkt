#lang racket

(require malt)

;(define (rank t)
;  (cond
;    ((scalar? t) 0)
;    (else (add1 (rank (tref t 0))))))

(define (rank t)
  (ranked t 0))

(define (ranked t a)
  (cond
    ((scalar? t) a)
    (else (ranked (tref t 0) (add1 a)))))

(define (shape t)
  (cond
    ((scalar? t) (list))
    (else (cons (tlen t) (shape (tref t 0))))))