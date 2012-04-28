; Aisleriot card games in Javascript
;
; Copyright (C) 2011-2012  Steven Price
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software

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

(define (and-map pred list)
	(if (null? list)
		#t
		(and (pred (car list))
                     (and-map pred (cdr list))
)))

; We treat vectors the same as lists
(define vector list)
(define (vector->list x) x)
(define (list->vector x) x)

; Else is the same as true when used in cond
(define else #t)

; These are just getting stupidly long
(define (cdaadr x) (cdaar (cdr x)))
(define (cadadr x) (cadar (cdr x)))

(define __game-list '(ten_across freecell klondike thirteen))
(set-statusbar-message (string-append "WebAisleriot "
			(javascript "version")
			" By Steven Price"))

(define (__game-options name nice-name)
 	(set-statusbar-message "Loading...")
	(load-file "sol.scm")
	(load-file name)
 	(set-statusbar-message "Choose options")
	((javascript "doOptions") nice-name)
)

