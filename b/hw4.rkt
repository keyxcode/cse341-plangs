
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride) 
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
    (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))]) 
            (car(list-tail xs i)))]))
