## The Little Learner

This repository contains the transcribed Racket code for all the chapters in the [The Little Learner](https://www.thelittlelearner.com/) book.

In most cases, some code from previous chapters is duplicated if referenced in the code for subsequent chapters. This means the code for each chapter can be run independently. (The exception is interlude 6, which re-uses some of the code from chapter 13.) At some point I may clean it up to avoid duplication of code.

The functions in the book are declared by defining a variable and assigning the body of the function to it with a lambda function. I use the procedure-defining syntax of define instead.

For example, I define the function shape like this:
```
(define (shape t)
  (cond
    ((scalar? t) (list))
    (else (cons (tlen t) (shape (tref t 0))))))
```
instead of this
```
(define shape
  (lambda (t)
    (cond
      ((scalar? t) (list))
      (else (cons (tlen t) (shape (tref t 0))))))
```
