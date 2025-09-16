#lang racket

(require malt)

(define (smooth decay-rate average g)
  (+ (* decay-rate average)
     (* (- 1.0 decay-rate) g)))

(smooth 0.9 (tensor 0.8 3.1 2.2) (tensor 1.0 1.1 3.0))