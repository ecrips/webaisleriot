(define passed 0)
(define failed 0)

(define (test name expected got)
  (if (equal? expected got)
      (set! passed (+ passed 1))
      (begin
        (set! failed (+ failed 1))
        (display (format #f "Test ~a failed: " name))
        (display (format #f "Got '~a' expected '~a'\n" got expected))
        )))

(test "equal1" #t (equal? "Foo" "Foo"))
(test "equal2" #f (equal? "Foo" "Bar"))

(test "format1" "Foo" (format #f "Foo"))
(begin
  (define number 0)
  (define (get-number)
    (begin
      (set! number (+ 1 number))
      number))
  (test "format2" "Foo 1 Bar 2"
        (format #f "Foo ~a Bar ~a" (get-number) (get-number)))
  )

(test "quote" 1 '1)

;;; Arithmetic
(test "add" 6 (+ 1 2 3))
(test "sub" 2 (- 5 2 1))
(test "mul" 24 (* 2 3 4))
(test "div" 2 (/ 12 3 2))
(test "quotient" 3 (quotient 10 3))
(test "modulo" 1 (modulo 10 3))
(test "max" 5 (max 1 5 3 2))
(test "min" 1 (min 1 5 3 2))
(test "expt" 8 (expt 2 3))
(test "integer-expt" 8 (integer-expt 2 3))

;;; Comparison and Logic
(test "equal-num" #t (= 5 5))
(test "equal-num-false" #f (= 5 6))
(test "greater" #t (> 6 5))
(test "greater-equal" #t (>= 6 6))
(test "less" #t (< 5 6))
(test "less-equal" #t (<= 5 5))
(test "not-true" #f (not #t))
(test "not-false" #t (not #f))
(test "and-true" #t (and #t #t))
(test "and-false" #f (and #t #f))
(test "and-multiple" 3 (and #t #t 3))
(test "or-true" #t (or #f #t))
(test "or-false" #f (or #f #f))
(test "or-multiple" 1 (or #f 1 #t))

;;; List operations
(test "cons" '(1 2 3) (cons 1 '(2 3)))
(test "car" 1 (car '(1 2 3)))
(test "cdr" '(2 3) (cdr '(1 2 3)))
(test "cadr" 2 (cadr '(1 2 3)))
(test "caddr" 3 (caddr '(1 2 3)))
(test "cadddr" 4 (cadddr '(1 2 3 4)))
(test "caar" 1 (caar '((1 2) 3)))
(test "cdar" '(2) (cdar '((1 2) 3)))
(test "caaar" 1 (caaar '(((1) 2) 3)))
(test "cadar" 2 (cadar '((1 2) 3)))
(test "cdaar" '(2) (cdaar '(((1 2)) 3)))
(test "null-true" #t (null? '()))
(test "null-false" #f (null? '(1)))
(test "length" 3 (length '(1 2 3)))
(test "reverse" '(3 2 1) (reverse '(1 2 3)))
(test "list-ref" 2 (list-ref '(1 2 3) 1))
(test "list-head" '(1 2) (list-head '(1 2 3) 2))
(test "list-tail" '(3) (list-tail '(1 2 3) 2))
(test "append" '(1 2 3 4) (append '(1 2) '(3 4)))
(test "member-found" '(2 3) (member 2 '(1 2 3)))
(test "member-not-found" #f (member 4 '(1 2 3)))
(test "map" '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
(test "list?" #t (list? '(1 2 3)))
(test "list?-false" #f (list? 1))
(test "equal-nested" #t (equal? '(1 (2 3) 4) '(1 (2 3) 4)))

;;; Sort
(test "sort" '(1 2 3) (sort '(3 1 2) <))

;;; Binding and Procedures
(test "let" 5 (let ((x 2) (y 3)) (+ x y)))
(test "let-named" 120 
      (let factorial ((n 5))
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))
(test "let*" 5 (let* ((x 2) (y (+ x 1))) (+ x y)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (#t (+ (fib (- n 1)) (fib (- n 2))))))
(test "recursion-fib" 5 (fib 5))

(test "apply" 6 (apply + '(1 2 3)))

;;; Conditionals
(test "if-true" 1 (if #t 1 2))
(test "if-false" 2 (if #f 1 2))
(test "cond-1" 1 (cond (#f 0) (#t 1) (#t 2)))
(test "cond-2" 2 (cond (#f 0) (#f 1) (#t 2)))

;;; Strings
(test "string-append" "foobar" (string-append "foo" "bar"))
(test "number->string" "123" (number->string 123))
(test "string<?" #t (string<? "abc" "def"))

;;; Hash Tables
(begin
  (define h (make-hash-table))
  (hash-set! h "a" 1)
  (hash-set! h "b" 2)
  (test "hash-ref-1" 1 (hash-ref h "a"))
  (test "hash-ref-2" 2 (hash-ref h "b"))
  (test "hash-ref-default" #f (hash-ref h "c")))

;;; Misc
(test "eq-empty-list" #t (eq? '() '()))
(begin
  (define r (random 10))
  (test "random-range" #t (and (>= r 0) (< r 10))))

(display (format #f "Passed: ~a Failed: ~a\n" passed failed))
(if (> failed 0) (display "Some tests failed\n") (display "All tests passed\n"))
