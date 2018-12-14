#lang plai
(define-type FAE
    [num    (n number?)]
    [add    (lhs FAE?) (rhs FAE?)]
    [sub    (lhs FAE?) (rhs FAE?)]
    [id     (name symbol?)]
    [fun    (param symbol?) (body FAE?)]
    [app    (ftn FAE?) (arg FAE?)])

(define-type FAE-Value
    [numV         (n number?)]
    [closureV    (param symbol?) (body FAE?) (ds DefrdSub?)])

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;[Contract] (numV numV -> numV) -> (FAE FAE -> FAE)
;[Purpose] To claculate add and sub more efficiently
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

;Solved by myself: Y
;[Contract] parse: sexp -> FAE
;[Purpose] to convert s-exp to FAE
;[time taken]: around 1 hours
;[Test] 1 ~ 10
;(test (interp (parse 3) (mtSub)) (numV 3))
;(test (interp (parse '{fun {x} {+ x 1}}) (mtSub)) (closureV 'x (add (id 'x) (num 1)) (mtSub)))
;(test/exn (interp (parse 'x) (mtSub)) "lookup: free identifier")
;(test (interp (parse '{+ 4 3}) (mtSub)) (numV 7))
;(test (interp (parse '{with {x 7} {- x 2}}) (mtSub)) (numV 5))
;(test (interp (parse '{with {x 10} {{fun {y} {+ y 1}} x}}) (mtSub)) (numV 11))
;(test (interp (parse '{with {x {- 20 5}} {{fun {y} {+ y 1}} x}}) (mtSub)) (numV 16))
;(test (interp (parse '{with {f {fun {x} {+ x x}}} {f 10}}) (mtSub)) (numV 20))
;(test (interp (parse '{with {f {fun {x} {+ x x}}} {f {- 10 5}}}) (mtSub)) (numV 10))
;(test (interp (parse '{with {f {fun {x} {+ x x}}} {+ 4 {f {- 10 5}}}}) (mtSub)) (numV 14))

(define (parse sexp)
 (match sexp
   [(? number?)		      (num sexp)]
   [(list '+ l r)	      (add (parse l) (parse r))]
   [(list '- l r)	      (sub (parse l) (parse r))]
   [(? symbol?)		      (id sexp)]
   [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
   [(list 'fun (list arg) b)  (fun arg (parse b))]
   [(list f a)		      (app (parse f) (parse a))]
   [else 	              (error 'parse "bad syntax: ~a" sexp)]))

;[Contract] lookup: symbol DefrdSub -> number
;[Purpose] to check free identifier and save bound identifier with it's value
(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub () (error 'lookup "free identifier")]
            [aSub (i v saved) (if (symbol=? i name) v (lookup name saved))]))

;[Contract] interp: FAE DefrdSub -> FAE-Value
;[Purpose] To get result
(define (interp fae ds)
    (type-case FAE fae
       [num (n) (numV n)]
       [add (l r) (num+ (interp l ds) (interp r ds))]
       [sub (l r) (num- (interp l ds) (interp r ds))]
       [id (s)    (lookup s ds)]
       [fun     (p b)  (closureV p b ds)]
       [app    (f a)   (local [(define f-val (interp f ds))
                                      (define a-val (interp a ds))]
                               (interp (closureV-body f-val)
                                           (aSub (closureV-param f-val)
                                                      a-val
                                                      (closureV-ds f-val))))]))



;[Test] 1 ~ 12
(test (interp (parse 3) (mtSub)) (numV 3))
(test (interp (parse '{fun {x} {+ x 1}}) (mtSub)) (closureV 'x (add (id 'x) (num 1)) (mtSub)))
(test/exn (interp (parse 'x) (mtSub)) "lookup: free identifier")
(test (interp (parse '{+ 4 3}) (mtSub)) (numV 7))
(test (interp (parse '{with {x 7} {- x 2}}) (mtSub)) (numV 5))
(test (interp (parse '{with {x 10} {{fun {y} {+ y 1}} x}}) (mtSub)) (numV 11))
(test (interp (parse '{with {x {- 20 5}} {{fun {y} {+ y 1}} x}}) (mtSub)) (numV 16))
(test (interp (parse '{with {f {fun {x} {+ x x}}} {f 10}}) (mtSub)) (numV 20))
(test (interp (parse '{with {f {fun {x} {+ x x}}} {f {- 10 5}}}) (mtSub)) (numV 10))
(test (interp (parse '{with {f {fun {x} {+ x x}}} {+ 4 {f {- 10 5}}}}) (mtSub)) (numV 14))

(test (interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub)) (closureV 'x (add (id 'y) (id 'x)) (aSub 'y (numV 10) (mtSub))))
(test (interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub)) (numV 7))

;(test (interp (parse '{{fun {x} 0} {+ 1 {fun {y} 2}}}) (mtSub)) (numV 0))