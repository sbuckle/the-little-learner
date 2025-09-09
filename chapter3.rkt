#lang racket

(require malt)

(define square sqr)

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))

(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

(define (line x)
  (λ (θ)
    (+ (* (ref θ 0) x) (ref θ 1))))

(define (l2-loss target)                                                            
  (λ (xs ys)
    (λ (θ)
      (let ((pred-ys ((target xs) θ)))
        (sum
         (square
          (- ys pred-ys)))))))

; (((l2-loss line) line-xs line-ys) (list 0.0 0.0))