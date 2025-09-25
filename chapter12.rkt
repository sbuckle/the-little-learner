#lang racket

(require malt)

(define (block fn shape-list)
  (list fn shape-list))

(define (block-fn ba)
  (ref ba 0))

(define (block-ls ba)
  (ref ba 1))

(define (dense-block n m)
  (block relu
         (list
          (list m n)
          (list m))))

(define (block-compose f g j)
  (λ (t)
    (λ (θ)
      ((g
        ((f t) θ))
       (refr θ j)))))

(define (stack2 ba bb)
  (block
   (block-compose
    (block-fn ba)
    (block-fn bb)
    (len (block-ls ba)))
   (append
    (block-ls ba)
    (block-ls bb))))

(define (stack-blocks bls)
  (stacked-blocks (refr bls 1) (ref bls 0)))

(define (stacked-blocks rbls ba)
  (cond
    ((null? rbls) ba)
    (else
     (stacked-blocks (refr rbls 1)
                     (stack2 ba (ref rbls 0))))))

(define layer1
  (dense-block 32 64))

(define layer2
  (dense-block 64 45))

(define layer3
  (dense-block 45 26))
                           