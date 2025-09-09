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

(define obj ((l2-loss line) line-xs line-ys))

; (obj (list -1.0 0.0))
; (obj (list -0.0 0.0))
; (obj (list 1.0 0.0))
; (obj (list 2.0 0.0))
; (obj (list 3.0 0.0))

; (gradient-of (λ (θ) (square (ref θ 0))) (list 27.0))

(define (revise f revs θ)
  (cond
    ((zero? revs) θ)
    (else (revise f (sub1 revs) (f θ)))))

(define revs 1000)
(define α 0.01)

(define (gradient-descent obj θ)
  (let ((f (λ (Θ)
             (map (λ (p g)
                    (- p (* α g)))
                  Θ
                  (gradient-of obj Θ)))))
    (revise f revs θ)))

;(gradient-descent obj '(0.0 0.0))
             
