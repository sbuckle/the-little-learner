#lang racket

(require malt)

;(define line
;  (λ (x)
;    (λ (θ)
;      (+ (* (ref θ 0) x) (ref θ 1)))))

(define (line x)
  (λ (θ)
    (+ (* (ref θ 0) x) (ref θ 1))))

;((line 7.3) (list 1.0 0.0))

(define line-xs
  ; [2.0 1.0 4.0 3.0]
  (tensor 2.0 1.0 4.0 3.0))

(define line-ys
  ; [1.8 1.2 4.2 3.3]
  (tensor 1.8 1.2 4.2 3.3))
