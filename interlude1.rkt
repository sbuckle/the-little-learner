#lang racket

(require malt)

(define (sum-1 t)
  (summed t (sub1 (tlen t)) 0.0))

(define (summed t i a)
  (cond
    ((zero? i) (+ (tref t 0) a))
    (else (summed t (sub1 i) (+ (tref t i) a)))))