
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
            (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
    (cond [(= n 0) null]
        [#t (let ([eval-s (s)])
            (cons (car eval-s) (stream-for-n-steps (cdr eval-s) (- n 1))))]))

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

(define (stream-add-zero s)
    (lambda () 
        (let ([eval-s (s)])
        (cons (cons 0 (car eval-s)) (stream-add-zero (cdr eval-s))))))

(define (cycle-lists xs ys)
    (letrec ([f (lambda (n) 
                (cons (cons (list-nth-mod xs n) 
                            (list-nth-mod ys n)) 
                        (lambda () (f (+ n 1)))))])
        (lambda () (f 0))))

(define (vector-assoc v vec)
    (letrec ([f (lambda (n) 
                (cond 
                    [(> n (- (vector-length vec) 1)) #f]
                    [(and (pair? (vector-ref vec n)) (equal? v (car (vector-ref vec n)))) (vector-ref vec n)]
                    [#t (f (+ n 1))]
                ))])
        (f 0)))

(define (cached-assoc xs n)
    (letrec([memo (make-vector n #f)]
            [idx 0]
            [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                    (if ans
                        ans
                        (let ([new-ans (assoc v xs)])
                            (begin
                            (vector-set! memo idx new-ans)
                            (set! idx (remainder (+ idx 1) n))
                            new-ans)))))])
    f))