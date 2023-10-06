#lang racket

(provide (all-defined-out))

(define x 3)
(define a 3)
(define y (+ x 2))

(define (pow x y)
    (if (= y 0)
        1
        (* x (pow x (- y 1)))))

(define (sum xs)
    (if (null? xs)
        0
        (+ (car xs) (sum (cdr xs)))))

(define (my-append xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (my-append (cdr xs) ys))))

(define (my-map f xs)
    (if (null? xs)
        null
        (cons (f (car xs)) (my-map f (cdr xs)))))