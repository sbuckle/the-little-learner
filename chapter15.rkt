#lang racket

(require malt)
(require malt/examples/morse)

(define (corr t)
  (λ (θ)
    (+ (correlate (ref θ 0) t) (ref θ 1))))

(define (recu t)
  (λ (θ)
    (rectify ((corr t) θ))))

(define (recu-block b m d)
  (block recu
         (list
          (list b m d)
          (list b))))

(define sum-2 sum-1)

(define sum-cols
  (ext1 sum-2 2))

(define (signal-avg t)
  (λ (θ)
    (/ (sum-cols t)
       (ref (refr (shape t) (- (rank t) 2)) 0))))

(define signal-avg-block
  (block signal-avg
         (list)))

(define (fcn-block b m d)
  (stack-blocks
   (list
     (recu-block b m d)
     (recu-block b m b))))

(define morse-fcn
  (stack-blocks
   (list
    (fcn-block 4 3 1)
    (fcn-block 8 3 4)
    (fcn-block 16 3 8)
    (fcn-block 26 3 16)
    signal-avg-block)))

(define (init-shape s)
  (cond
    ((= (len s) 1) (zero-tensor s))
    ((= (len s) 2)
     (random-tensor 0.0 (/ 2 (ref s 1)) s))
    ((= (len s) 3)
     (random-tensor 0.0
                    (/ 2 (* (ref s 1) (ref s 2))) s))))

(define (init-θ shapes)
  (map init-shape shapes))

(define (trained-morse classifier θ-shapes)
  (model classifier
         (adam-gradient-descent
          (sampling-obj
           (l2-loss classifier)
           morse-train-xs
           morse-train-ys)
          (init-θ θ-shapes))))

(define (train-morse network)
  (with-hypers
      ((alpha 0.0005)
       (revs 20000)
       (batch-size 8)
       (mu 0.9)
       (beta 0.999))
    (trained-morse
     (block-fn network)
     (block-ls network))))

(define (skip f j)
  (λ (t)
    (λ (θ)
      (+ ((f t) θ)
         ((correlate (ref θ j)) t)))))

(define (skip-block ba d b)
  (let ((shape-list (block-ls ba)))
    (block
     (skip (block-fn ba) (len shape-list))
     (append
      shape-list
      (list
       (list b 1 d))))))

(define (residual-block b m d)
  (skip-block
   (fcn-block b m d)
   d b))

(define morse-residual
  (stack-blocks
   (list
    (residual-block 4 3 1)
    (residual-block 8 3 4)
    (residual-block 16 3 8)
    (residual-block 26 3 16)
    signal-avg-block)))