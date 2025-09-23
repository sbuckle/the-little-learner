#lang racket

(require malt)

(define (rectify-0 s)
  (cond
    ((< s 0.0) 0.0)
    (else s)))

(define rectify
  (ext1 rectify-0 0))

(define (linear-1-1 t)
  (λ (θ)
    (+ (dot-product (ref θ 0) t) (ref θ 1))))

(define (relu-1-1 t)
  (λ (θ)
    (rectify ((linear-1-1 t) θ))))