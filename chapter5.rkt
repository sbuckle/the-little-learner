#lang racket

(require malt)

(define square sqr)

(define (sum-1 t)
  (summed t (sub1 (tlen t)) 0.0))

(define (summed t i a)
  (cond
    ((zero? i) (+ (tref t 0) a))
    (else (summed t (sub1 i) (+ (tref t i) a)))))

(define (l2-loss target)                                                            
  (λ (xs ys)
    (λ (θ)
      (let ((pred-ys ((target xs) θ)))
        (sum
         (square
          (- ys pred-ys)))))))

(define (revise f revs θ)
  (cond
    ((zero? revs) θ)
    (else (revise f (sub1 revs) (f θ)))))

(declare-hyper revs)

(declare-hyper α)

(define (gradient-descent obj θ)
  (let ((f (λ (Θ)
             (map (λ (p g)
                    (- p (* α g)))
                  Θ
                  (gradient-of obj Θ)))))
    (revise f revs θ)))

(define quad-xs
  (tensor -1.0 0.0 1.0 2.0 3.0))

(define quad-ys
  (tensor 2.55 2.1 4.35 10.2 18.25))

;; ax^2 + bx + c = 0
(define (quad t)
  (λ (θ)
    (+ (* (ref θ 0) (square t))
       (+ (* (ref θ 1) t) (ref θ 2)))))

;; ((quad 3.0) (list 4.5 2.1 7.8))

;; (with-hypers
;;       ((revs 1000)
;;        (α 0.001))
;;     (gradient-descent
;;      ((l2-loss quad) quad-xs quad-ys)
;;      (list 0.0 0.0 0.0)))

(define plane-xs
  (tensor
   (tensor 1.0 2.05)
   (tensor 1.0 3.0)
   (tensor 2.0 2.0)
   (tensor 2.0 3.91)
   (tensor 3.0 6.13)
   (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(define (plane t)
  (λ (θ)
    (+ (dot-product (ref θ 0) t) (ref θ 1))))

(define (dot-product-1-1 w t)
  (sum-1
   (* w t)))

;;  (with-hypers
;;       ((revs 1000)
;;        (α 0.001))
;;     (gradient-descent
;;      ((l2-loss plane) plane-xs plane-ys)
;;      (list (tensor 0.0 0.0) 0.0)))