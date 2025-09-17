#lang racket

(require malt)

(define square sqr)

(declare-hyper α)

(declare-hyper β)

(declare-hyper μ)

(define ϵ 1e-8)

(define (smooth decay-rate average g)
  (+ (* decay-rate average)
     (* (- 1.0 decay-rate) g)))

(define (rms-u P g)
  (let ((r (smooth β (ref P 1) (square g))))
    (let ((α-hat (/ α (+ (sqrt r) ϵ))))
      (list (- (ref P 0) (* α-hat g)) r))))

(define (rms-i p)
  (list p (zeroes p)))

(define (rms-d P)
  (ref P 0))

(define rms-gradient-descent
  (gradient-descent rms-i rms-d rms-u))

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

(define (try-plane a-gradient-descent a-revs an-α)
  (with-hypers
      ((revs a-revs)
       (α an-α)
       (batch-size 4))
    (a-gradient-descent
     (sampling-obj
      (l2-loss plane) plane-xs plane-ys)
     (list (tensor 0.0 0.0) 0.0))))

;; (with-hypers
;;     ((β 0.9))
;;   (try-plane rms-gradient-descent 3000 0.01))

(define (adam-u P g)
   (let ((r (smooth β (ref P 2) (square g))))
     (let ((α-hat (/ α (+ (sqrt r) ϵ)))
           (v (smooth μ (ref P 1) g)))
       (list (- (ref P 0) (* α-hat v)) v r))))

(define (adam-i p)
  (let ((v (zeroes p)))
    (let ((r v))
      (list p v r))))

(define (adam-d P)
  (ref P 0))

(define adam-gradient-descent
  (gradient-descent adam-i adam-d adam-u))

;; (with-hypers
;;     ((μ 0.85)
;;      (β 0.9))
;;   (try-plane adam-gradient-descent 1500 0.01))