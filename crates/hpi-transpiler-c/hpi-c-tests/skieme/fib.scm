; === Fib ===
(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
            (fib (- n 2))
        )
    )
)
;; (display (fib 10))
;; (newline)

; === Fac ===
(define (fac n)
    (if (= n 0)
        1
        (* n (fac (- n 1) ))
    )
)
;; (display (fac 4))
;; (newline)

;; (define run (list fib fac))
;; (define (runner lst) () )

; === List stuff ===
(define x (list 1 2 3 4 5))
;(display (cddddr x))
;(newline)

;; (define y '(1 ( 2 69 ) 3))
;; (display (cadadr y))
;; (newline)

;; (define z '(1 ( 2 (69 99)) 3))
;; (display (car (cadadr z)))
;; (newline)

(define (min-element x y) (if (< x y) x y))

(define (minimum_ lst)
    (if (null? (cdr lst))
        (car lst)
        (min-element (car lst) (minimum_ (cdr lst)))
    )
)

(define (srt lst)
    (define min_ (minimum_ lst))
    (define not_eq? (lambda (n) (not (equal? min_ n))))
    (define rest (filter not_eq? lst))

    (if (null? rest)
      (list min_)
      (cons min_ (srt rest))
    )
)


(define (map_ predicate container)
    (define this (predicate (car container)))

    (if (null? (cdr container))
        (list this)
        (cons this (map_ predicate (cdr container)))
    )
)

(define (filter_ predicate container)
    (define this (car container))
    (define this_mod
        (if (predicate this)
            (list this)
            '()
        )
    )

    (if (null? (cdr container))
        this_mod
        (append this_mod (filter_ predicate (cdr container)))
    )
)

;; (display (minimum_ (list 9 2 3 4)))
;; (display (srt (list 1 9 3 5)))

(define input (list 1 6 9 3 8))

;;(display (srt (list 3 2 1)))
;;(display (srt (list 3 5 2 1)))
;; (define lazy (delay (+ 1 2)))
;; (display (force lazy))

(display input)
(newline)

(define (double n) (* 2 n))
;; (define (is_even n) (= (% n 2) 0))

(display (map_ double input))

(newline)

(display (even? 42))

(newline)

(define gerade? (lambda (n) (not (odd? n))))
(define ungerade? (lambda (n) (not (even? n))))

(define (fib2 n)
    (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1)))
    )
)

(display (fib2 10))
(newline)

(display (filter_ gerade? input))

;;(display (srt (list 3 2 1)))
;;(display (srt (list 3 5 2 1)))
;; (define lazy (delay (+ 1 2)))
;; (display (force lazy))
(newline)
