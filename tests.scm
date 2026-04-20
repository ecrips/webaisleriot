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

(display (format #f "Passed: ~a Failed: ~a\n" passed failed))
(if (> failed 0) (display "Some tests failed\n") (display "All tests passed\n"))
