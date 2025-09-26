#lang racket

(require malt)
(require malt/examples/iris)

(define (dense-block n m)
  (block relu
         (list
          (list m n)
          (list m))))

(define iris-network
  (stack-blocks
   (list
    (dense-block 4 6)
    (dense-block 6 3))))

(define iris-classifier
  (block-fn iris-network))

(define iris-θ-shapes
  (block-ls iris-network))

(define (init-shape s)
  (cond
    ((= (len s) 1) (zero-tensor s))
    ((= (len s) 2)
     (random-tensor 0.0 (/ 2 (ref s 1)) s))))

(define (init-θ shapes)
  (map init-shape shapes))

(define iris-θ
  (with-hypers
      ((revs 2000)
       (alpha 0.0002)
       (batch-size 8))
    (naked-gradient-descent
     (sampling-obj
      (l2-loss iris-classifier)
      iris-train-xs iris-train-ys)
     (init-θ iris-θ-shapes))))

(define (model target θ)
  (λ (t)
    ((target t) θ)))

(define iris-model
  (model iris-classifier iris-θ))