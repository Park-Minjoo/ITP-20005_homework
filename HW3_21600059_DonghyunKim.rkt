#lang plai

(define-type WAE
	[num (n number?)]
	[add (lhs WAE?) (rhs WAE?)]
	[sub (lhs WAE?) (rhs WAE?)]
	[with (name symbol?) 
             (named-expr WAE?)
             (body WAE?)]
	[id (name symbol?)])

;; [contract] parse: sexp -> WAE
;; [purpose] to convert s-expression into WAE
;(define (parse sexp)
;  (match sexp
;   	[(? number?) 			(num sexp)]
;   	[(list '+ l r)			(add (parse l)	(parse r))]
;   	[(list '- l r)			(sub (parse l)	(parse r))]
;    	[(list 'with (list i v) e)	(with i (parse v) (parse e))];
;	[(? symbol?)			(id sexp)]
;   	[else (error 'parse "bad syntax:~a" sexp)]))

; [contract] subst: WAE symbol number -> WAE
(define (subst wae idtf val)
	(type-case WAE wae
		[num (n)	wae]
		[add (l r) 	(add (subst l idtf val) (subst r idtf val))]
		[sub (l r)	(sub (subst l idtf val) (subst r idtf val))]
		[with (i v e) 	(with i (subst v idtf val)
                                      (if (symbol=? i idtf) e
						(subst e idtf val)))]
		[id (s) 	(if (symbol=? s idtf) (num val) wae)])) ;need to fix

;{with x 10} {.... {with y 17} x}

;; interp: AE -> number
(define (interp wae)
	(type-case WAE wae
		[num (n) n]
		[add (l r) (+ (interp l) (interp r))]
		[sub (l r) (- (interp l) (interp r))]
		[with (i v e) (interp (subst e i (interp v)))]
                [id (s)		(error 'interp "free identifier")]))


(define (interp2 wae)
	(type-case WAE wae
		[num (n) n]
		[add (l r) (+ (interp2 l) (interp2 r))]
		[sub (l r) (- (interp2 l) (interp2 r))]
		[with (i v e) (interp2 (subst e i (interp2 v)))]
                [id (s)	s]))


(define (free-ids wae)
  (type-case WAE wae
    [num (n) empty]
    [id (idtf) (cons idtf empty)]
    [add (l r) (append (free-ids l) (free-ids r))]
    [sub (l r) (append (free-ids l) (free-ids r))]
    [with (i v e) (free-ids (subst e i (interp2 v)))]
    ))


(test (free-ids (id 'y)) '(y))
(test (free-ids (add (id 'y) (num 4))) '(y))
(test (free-ids (with 'x (num 3) (id 'y))) '(y))
(test (free-ids (with 'x (num 3) (add (id 'y) (num 1)))) '(y))
(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(b a))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(y b a))



;(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(y b a t))
;(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(y x))
;(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(y c b a))
;(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(y c b d))
;(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(y c b z d))

