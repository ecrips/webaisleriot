; Library procedures
(define (zero? a) (= a 0))

(define (_ x) x)

(define $expt expt)

(define (version) 0.1)
(define (use-modules x) #t)
(define (ice-9 x) #f)
(define format #f)

(define (boolean? x) (or (eq? x #t) (eq? x #f)))

(define (add-to-score! x) (set-score! (+ (get-score) x)))

(define (or-map pred list)
	(if (null? list)
		#f
		(or (pred (car list))
		    (or-map pred (cdr list))
)))

; We treat vectors the same as lists
(define vector list)
(define (vector->list x) x)
(define (list->vector x) x)

; Else is the same as true when used in cond
(define else #t)

; These are just getting stupidly long
(define (cdaadr x) (cdaar (cdr x)))

;(load-file "sol.scm")
;(load-file "ten_across.scm")

(define __game-list '(ten_across freecell))
;(define __game-list '(ten_across))
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
