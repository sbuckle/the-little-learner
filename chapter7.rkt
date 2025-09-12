#lang racket

(require malt)

(declare-hyper revs)

(declare-hyper α)

(define (gradient-descent inflate deflate update)
  (λ (obj θ)
    (let ((f (λ (Θ)
               (map update
                    Θ
                    (gradient-of obj
                                 (map deflate Θ))))))
      (map deflate
           (revise f revs
                   (map inflate θ))))))

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

(define (try-plane a-gradient-descent)
  (with-hypers
      ((revs 15000)
       (α 0.001)
       (batch-size 4))
    (a-gradient-descent
     (sampling-obj
      (l2-loss plane) plane-xs plane-ys)
     (list (tensor 0.0 0.0) 0.0))))

(define (lonely-i p)
  (list p))

(define (lonely-d P)
  (ref P 0))

(define (lonely-u P g)
    (list (- (ref P 0) (* α g))))

(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))

;; (try-plane lonely-gradient-descent)

(define (naked-i p)
  (let ((P p))
    P))

(define (naked-d P)
  (let ((p P))
    p))

(define (naked-u P g)
  (- P (* α g)))
  
(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))

;; (try-plane naked-gradient-descent)