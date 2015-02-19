(define lang #f)
(define scheme #f)
(define racket #f)
(define (planet n) #f)
(define jaymccarthy/sqlite:5:1/sqlite #f)
(define (require . args) #f)
(define (provide . args) #f)
(define (all-defined-out) #f)

(define (make-semaphore n) #f)
(define (semaphore-wait n) #f)
(define (semaphore-post n) #f)

;; tinyscheme
(define db-select db-exec)

;; helper to return first instance from a select
(define (select-first db str . args)
  (let ((s (apply db-select (append (list db str) args))))
    (if (or (null? s) (eq? s #t))
        '()
        (vector-ref (cadr s) 0))))

;; get a unique hash for this user (used for all the unique-ids)
(define (get-unique user)
  (let ((t (time-of-day)))
    (string-append
     user "-" (number->string (car t)) ":" (number->string (cadr t)))))
