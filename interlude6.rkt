#lang racket

(require malt)
(require malt/examples/iris)

(require "chapter13.rkt")

(define (argmax-1 t)
  (let ((i (sub1 (tlen t))))
    (argmaxed t i i)))

(define (next-a t i a)
  (cond
    ((> (tref t i) (tref t a)) i)
    (else a)))

(define (argmaxed t i a)
  (let ((a-hat (next-a t i a)))
    (cond
      ((zero? i) a-hat)
      (else (argmaxed t (sub1 i) a-hat)))))

(define (class=-1 t u)
  (cond
    ((= (argmax-1 t) (argmax-1 u)) 1.0)
    (else 0.0)))

(define class=
  (ext2 class=-1 1 1))

(define (accuracy a-model xs ys)
  (/ (sum (class= (a-model xs) ys))
     (tlen xs)))

;; (accuracy iris-model iris-test-xs iris-test-ys)

(define (accurate-enough-iris-θ? θ)
  (>= (accuracy
       (model iris-classifier θ)
       iris-test-xs iris-test-ys)
      0.9))

;; (grid-search
;;  accurate-enough-iris-θ?
;;  ((revs 500 1000 2000 4000)
;;   (alpha 0.0001 0.0002 0.0005)
;;   (batch-size 4 8 16))
;;  (naked-gradient-descent
;;   (sampling-obj
;;    (l2-loss iris-classifier)
;;    iris-train-xs iris-train-ys)
;;   (init-θ iris-θ-shapes)))