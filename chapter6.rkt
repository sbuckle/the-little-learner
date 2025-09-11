#lang racket

(require malt)

;; (declare-hyper revs)

;; (declare-hyper α)

(declare-hyper batch-size)

;; (define line-xs
;;   (tensor 2.0 1.0 4.0 3.0))
;; 
;; (define line-ys
;;   (tensor 1.8 1.2 4.2 3.3))
;; 
;; (define (gradient-descent obj θ)
;;   (let ((f (λ (Θ)
;;              (map (λ (p g)
;;                     (- p (* α g)))
;;                   Θ
;;                   (gradient-of obj Θ)))))
;;     (revise f revs θ)))

(define (samples n s)
  (sampled n s (list)))

(define (sampled n i a)
  (cond
    ((zero? i) a)
    (else
     (sampled n (sub1 i)
              (cons (random n) a)))))

(define (sampling-obj expectant xs ys)
  (let ((n (tlen xs)))
    (λ (θ)
      (let ((b (samples n batch-size)))
        ((expectant (trefs xs b) (trefs ys b)) θ)))))

;; (with-hypers
;;     ((revs 1000)
;;      (α 0.01)
;;      (batch-size 4))
;;   (gradient-descent
;;    (sampling-obj
;;     (l2-loss line) line-xs line-ys)
;;    (list 0.0 0.0)))