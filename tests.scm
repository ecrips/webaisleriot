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

(display (format #f "Passed: ~a Failed: ~a\n" passed failed))
(if (> failed 0) (display "Some tests failed\n") (display "All tests passed\n"))
