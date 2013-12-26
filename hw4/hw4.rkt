
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(empty? xs) (error "list-nth-mod: empty list")]
          [#t (car (list-tail xs 
                    (remainder n (length xs))))]
          ))


(define (stream-for-n-steps s n)
    (if (= n 0) null 
        (let [(pr (s))]
            (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1)))
)))

(define funny-number-stream 
    (letrec ([helper (lambda (x) 
      (cons  (if (= (remainder x 5) 0) (- x) x)  
             (lambda () (helper (+ x 1)))))])
    (lambda () (helper 1))))

(define dan-then-dog
  (letrec ([dog-then-dan (lambda ()
        (cons "dog.jpg" dan-then-dog))])
    (lambda () (cons "dan.jpg" dog-then-dan))))

(define (stream-add-zero s)
  (letrec ([second (s)])
     (lambda () 
      (cons (cons 0 (car second))
            (lambda () (stream-add-zero (cdr second)))))))

; pass
(define (cycle-lists xs ys)
  (define (helper n)
    (lambda () (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (helper (+ n 1)))))
  (helper 0))

;pass
(define (vector-assoc v vec)
  (define (helper n)
    (if (>= n (vector-length vec)) #f
      (let ([elem (vector-ref vec n)])
        (cond [(and (pair? elem) 
                    (equal? (car elem) v)) elem]
              [#t (helper (+ n 1))]))))
  (helper 0))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n 0)]
           [insert-pos 0]
           [f (lambda (v)
                 (let ([ans (vector-assoc v memo)])
                    (if ans ans
                        (let ([new-ans (assoc v xs)])
                            (begin 
                                (vector-set! memo insert-pos new-ans)
                                (set! insert-pos (remainder (+ insert-pos 1) n))
                                new-ans
                                )))))
           ])
  f))

(define-syntax while-less
  (syntax-rules (do)
      [(while-less e1 do e2)
       (letrec ([thunk (lambda ()
                          (let ([val e2])
                            (if (< val e1) (thunk) #t)
        ))])
       (thunk)
)]))



