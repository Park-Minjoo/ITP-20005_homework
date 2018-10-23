#lang plai

(define-type WAE
 	[num (n number?)]
	[add (lhs WAE?) (rhs WAE?)]
	[sub (lhs WAE?) (rhs WAE?)]
	[with (name symbol?) 
             (named-expr WAE?)
             (body WAE?)]
	[id (name symbol?)]
  )

(define (subst wae idtf val)
	(type-case WAE wae
		[num (n)	wae]
		[add (l r) 	(add (subst l idtf val) (subst r idtf val))]
		[sub (l r)	(sub (subst l idtf val) (subst r idtf val))]
		[with (i v e) 	(with i (subst v idtf val)
                                      (if (symbol=? i idtf) e
						(subst e idtf val)))]
		[id (s) 	(if (symbol=? s idtf) (num 0) wae)]))

(define (interp wae)
	(type-case WAE wae
		[num (n) n]
		[add (l r) (+ (interp l) (interp r))]
		[sub (l r) (- (interp l) (interp r))]
		[with (i v e) (interp (subst e i (interp v)))]
		[id (s)		(error 'interp "free identifier")]))



(define (free-ids wae)
	(type-case WAE wae
		[num (n) empty]
		[add (l r) (sort (remove-duplicates (append (free-ids l) (free-ids r)))symbol<?)]
		[sub (l r) (sort (remove-duplicates (append (free-ids l) (free-ids r)))symbol<?)]
		[with (i v e) (sort (remove-duplicates (append (remove* (list i) (free-ids e)) (free-ids v)))symbol<?)]
		[id (s)		(list s)]))

(free-ids (with 'x (num 5) (add (id 'x) (with 'y (id 'z) (id 'x)))))
(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))