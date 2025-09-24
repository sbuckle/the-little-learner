#lang racket

(require malt)

(define (dot-product-2-1 w t)
  (sum
   (*-2-1 w t)))

(define (linear t)
  (λ (θ)
    (+ (dot-product-2-1 (ref θ 0) t) (ref θ 1))))

(define (relu t)
  (λ (θ)
    (rectify ((linear t) θ))))

(define (k-relu k)
  (λ (t)
    (λ (θ)
      (cond
        ((zero? k) t)
        (else (((k-relu (sub1 k))
                ((relu t) θ)
                (refr θ 2))))))))