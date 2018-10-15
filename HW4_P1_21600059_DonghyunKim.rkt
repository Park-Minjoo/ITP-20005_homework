#lang plai

(define-type FWAE
    [num    (n number?)]
    [add    (lhs FWAE?) (rhs FWAE?)]
    [sub    (lhs FWAE?) (rhs FWAE?)]
    [with   (name symbol?) (named-expr FWAE?) (body FWAE?)]
    [id     (name symbol?)]
    [fun    (param symbol?) (body FWAE?)]
    [app    (ftn FWAE?) (arg FWAE?)])

;Solved by myself: Y
;[Contract] parse: sexp -> FWAE
;[Purpose] to convert s-exp to FWAE
;[time taken]: around 3 hours
;[Test] 1 ~ 10
;(test (interp (parse 3)) (num 3))
;(test (interp (parse '{fun {x} {+ x 1}})) (fun 'x (add (id 'x) (num 1))))
;(test/exn (interp (parse 'x)) "interp: free identifier")
;(test (interp (parse '{+ 4 3})) (num 7))
;(test (interp (parse '{with {x 7} {- x 2}})) (num 5))
;(test (interp (parse '{with {x 10} {{fun {y} {+ y 1}} x}})) (num 11))
;(test (interp (parse '{with {x {- 20 5}} {{fun {y} {+ y 1}} x}})) (num 16))
;(test (interp (parse '{with {f {fun {x} {+ x x}}} {f 10}})) (num 20))
;(test (interp (parse '{with {f {fun {x} {+ x x}}} {f {- 10 5}}})) (num 10))
;(test (interp (parse '{with {f {fun {x} {+ x x}}} {+ 4 {f {- 10 5}}}})) (num 14))
(define (parse sexp)
 (match sexp
   [(? number?)		      (num sexp)]
   [(list '+ l r)	      (add (parse l) (parse r))]
   [(list '- l r)	      (sub (parse l) (parse r))]
   [(list 'with (list i v) e) (with i (parse v) (parse e))]
   [(? symbol?)		      (id sexp)]
   [(list 'fun (list arg) b)  (fun arg (parse b))]
   [(list f a)		      (app (parse f) (parse a))]
   [else 	              (error 'parse "bad syntax: ~a" sexp)]))


;[Contract] (num num -> num) -> (FWAE FWAE -> FWAE)
;[Purpose] To claculate add and sub more efficiently
(define (num-op op)
     (lambda (x y)
          (num (op (num-n x) (num-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

;[Contract] subst: FWAE symbol number -> FWAE
;[Purpose] to support interp
(define (subst exp idtf val)
     (type-case FWAE exp
       [num (n)	exp]
       [add (l r) (add (subst l idtf val) (subst r idtf val))]
       [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
       [with (i v e) 	(with i (subst v idtf val)
                              (if (symbol=? i idtf) e
                                  (subst e idtf val)))]
       [id (name) (cond
                    [(equal? name idtf) val]
                    [else exp])]
       [app (f arg) (app (subst f idtf val) (subst arg idtf val))]
       [fun (id body) (if (equal? idtf id) exp (fun id (subst body idtf val)))]))

;[Contract] interp: FWAE-> FWAE
;[Purpose] To get result
(define (interp fwae)
     (type-case FWAE fwae
          [num   (n)        fwae]
          [add    (l r)     (num+ (interp l) (interp r))]
          [sub    (l r)     (num- (interp l) (interp r))]
          [with   (i v e)   (interp (subst e i (interp v)))]
          [id     (s)       (error 'interp "free identifier")]
          [fun    (p b)     fwae]
          [app    (f a)     (local [(define ftn (interp f))]
                                        (interp (subst (fun-body ftn)
                                                  (fun-param ftn)  
                                                  (interp a))))]))


;[Test] 1 ~ 10
(test (interp (parse 3)) (num 3))
(test (interp (parse '{fun {x} {+ x 1}})) (fun 'x (add (id 'x) (num 1))))
(test/exn (interp (parse 'x)) "interp: free identifier")
(test (interp (parse '{+ 4 3})) (num 7))
(test (interp (parse '{with {x 7} {- x 2}})) (num 5))
(test (interp (parse '{with {x 10} {{fun {y} {+ y x}} x}})) (num 20))
(test (interp (parse '{with {x {- 20 5}} {{fun {y} {+ y x}} {+ x 2}}})) (num 32))
(test (interp (parse '{with {f {fun {x} {+ x x}}} {f 10}})) (num 20))
(test (interp (parse '{with {f {fun {x} {+ x x}}} {f {- 10 5}}})) (num 10))
(test (interp (parse '{with {f {fun {x} {+ x x}}} {+ 4 {f {- 10 5}}}})) (num 14))
(test (parse '{{fun {x} {+ x 1}} 10})
                    (app (fun 'x (add (id 'x) (num 1))) (num 10)))