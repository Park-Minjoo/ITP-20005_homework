#lang plai

;(require racket/trace)


;Problem 1 & 2:
;Solved by myself: Y
;Time taken: around 10hours(Problem 1: 8~9, Problem 2: 1)
;[contract] WAE->list-of-symbols(problem 2: sort and eliminate duplication)
;[purpose] to find out free identifier from abstract syntax
;[tests]
;(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
;(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
;(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
;(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
;(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
;(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
;(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
;(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
;(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
;(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))


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
		[id (s) 	(if (symbol=? s idtf) (if (number? val) (num val) (id val)) wae)]
          ))

(define (interp wae)
	(type-case WAE wae
		[num (n)  n]
		[add (l r) (if (and (not(number? (interp l))) (not(number? (interp r)))) (+ (interp l) (interp r)) (if (number? (interp l)) (interp r) (interp l)))]
		[sub (l r) (if (and (not(number? (interp l))) (not(number? (interp r)))) (- (interp l) (interp r)) (if (number? (interp l)) (interp r) (interp l)))]
		[with (i v e) (interp (subst e i (interp v)))]
                [id (s)	 s]))



(define (free-ids wae)
  (type-case WAE wae
    [num (n) empty]
    [add (l r) (remove-duplicates (sort (append (free-ids l) (free-ids r)) symbol<?))]
    [sub (l r) (remove-duplicates (sort (append (free-ids l) (free-ids r)) symbol<?))]
    [with (i v e) (remove-duplicates (sort (append (free-ids (subst e i (interp v))) (free-ids v)) symbol<?))]
    [id (idtf) (cons idtf empty)]
    ))

;(trace free-ids)
;(trace interp)
;(trace subst)

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