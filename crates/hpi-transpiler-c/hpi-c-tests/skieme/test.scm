;(define (fib n)
;  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))
;)

;(display (fib 10))
;(newline)

;(define sub2 (lambda (n) (- n 2)))
;(display (sub2 10))
;(newline)


;; (define x (list 1 2 3 4 5 6 7 8 9 10) )
;(define map_f (lambda (f) * f 2))
;; (define (times2 f) (* f 2))
;(display (map fib x))


(define ppl (list "Ernst" "Neyer" "Bambini" "Femboy" "Kollege" "Macbook" "Wooooow" "Sumpfboot"))
;; (define (greet p) (string-append "<Ehlo: " p "!>"))
;; (display (map greet ppl))

;; (define (add_plus x y) (+ (+ x y) 1))
;;
;; (use-modules (srfi srfi-1))
;; (display (fold add_plus 10 x))
;; (display (andmap positive? (list 1 2 3 4 5 6 7 8)))

;; (define y (cons 1 ( cons 2 ( cons 3 '() ) )))
;; (display y)
;; (newline)
