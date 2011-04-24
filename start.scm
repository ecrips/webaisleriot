; Library procedures
(define (zero? a) (= a 0))

(define (_ x) x)

;(load-file "sol.scm")
;(load-file "ten_across.scm")

(define __game-list '(ten_across))
(set-statusbar-message "Select a game")

(define (__game-options name nice-name)
 	(set-statusbar-message "Loading...")
	(load-file "sol.scm")
	(load-file name)
 	(set-statusbar-message "Choose options")
	((javascript "doOptions") nice-name)
)

;(define d set-statusbar-message)
;
;(define (make-card a b) (list a b))
;
;(define (test value suit)
;	(make-card value suit))
;
;(d (test 'test 'bob))

;(d
;(let ((x 2) (y 3))
;  (let* ((x 7)
;         (z (+ x y)))
;    (* z x)))
;) ; ==> 70
;(d
;
;(let ((x 2) (y 3))
;  (let ((x 7)
;        (z (+ x y)))
;    (* z x)))
;) ; ==> 35

;(d (cons 1 (cons 2 (cons 3 4))))
;(d (list 1 2 3 4))
