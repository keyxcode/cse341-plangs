
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

(define (stream-for-n-steps s n)
    (cond [(= n 0) null]
        [#t (let ([eval_s (s)])
            (cons (car eval_s) (stream-for-n-steps (cdr eval_s) (- n 1))))]))

(define funny-number-stream
    (letrec ([f (lambda (x) (let ([num (if (= 0 (remainder x 5))
                                            (- 0 x)
                                            x)]) 
                            (cons num (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog 
    (letrec ([f (lambda (x) (let ([data (if (= 0 (remainder x 2)) "dan.jpg" "dog.jpg")]) 
                            (cons data (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))