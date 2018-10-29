#lang plai

; Solve by myself: ...
; Time taken: ...

; Type definition for abstract syntax tree of LFAE
(define-type LFAE
    [num    (n number?)]
    [add     (lhs LFAE?) (rhs LFAE?)]
    [sub     (lhs LFAE?) (rhs LFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body LFAE?)]
    [app     (ftn LFAE?) (arg LFAE?)])
  
        
; parse: sexp -> LFAE
; purpose: to convert sexp to LFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))


;(parse '{1 2})
;(parse '{with {x 3} {+ x y}}) ; {{fun {x} {+ x y}} 3}
;(parse '{{fun {a} {+ 3 a}} 10})
;(parse '{+ 2 {fun {a} {+ 3 a}}})
;(test (parse '{{fun {x} {+ x 1}} 10})
;                   (app (fun 'x (add (id 'x) (num 1))) (num 10)))

; Type definition for deferred substitution
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])

; Type Defintion for LFAE-Value
(define-type LFAE-Value
  [numV       (n number?)]
  [closureV   (param symbol?) (body LFAE?) (ds DefrdSub?)]
  [exprV      (expr LFAE?) (ds DefrdSub?)])

; strict: ...
; purpose: ...
(define (strict v)
    (type-case LFAE-Value v
        [exprV (expr ds) (strict (interp expr ds))]
        [else v]))


; num-op: operators for artithmatic computation -> function for artithmatic computation
; purpose: to get a function for arithmatic computation.
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n (strict x)) (numV-n (strict y))))))

(define num+ (num-op +))
(define num- (num-op -))

; lookup: ...
; purpose: ...
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i v saved) (if(symbol=? i name)
                                v             ;; if v is exprV (num ==> interp it
                                (lookup name saved))]))

; interp: ...
; purpose: ...
(define (interp lfae ds)
  (type-case LFAE lfae
     [num (n)      (numV n)]
     [add (l r)    (num+ (interp l ds) (interp r ds))]
     [sub (l r)    (num- (interp l ds) (interp r ds))]
     [id  (s)     (lookup s ds)]
     [fun (p b)  (closureV p b ds)]
     [app (f a)   (local [(define f-val (strict (interp f ds)))
                          (define a-val (exprV a ds))]
                   (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val))))]))
; run: ...
; purpose: ...
(define (run sexp ds)
     (interp (parse sexp) ds))

;(run '{fun {y} {+ x y}} (mtSub))
;(run '{{fun {x} {+ 1 x}} 10} (mtSub))
;(run '{{fun {x} 0} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{+ 1 {fun {y} 2}} (mtSub))
;(run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{{fun {x} x} {+ 1 {{fun {y} 2} 1}}} (mtSub))
;(run '{{fun {x} x} {+ 1 1}} (mtSub))


;(run '{{fun {x} {+ x x}} 1} (mtSub))

;(run '{{fun {x} {+ x x}} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
;(run '{{fun {x} {+ x x}} 1} (mtSub))

; run #1
(run '{{fun {x} x} 1} (mtSub))

; run #2
; How many times is {- {+ 4 5} {+ 8 9}} computed in the interpreter? (              )
; Why is the expression, {- {+ 4 5} {+ 8 9}}, computed several times? (                                          )
(run '{{fun {x} {+ {+ x x} {+ x x}}} {- {+ 4 5} {+ 8 9}}} {mtSub})


; test case #1
(test (run '{{fun {x} x} 1} (mtSub)) (numV 1))

; test case #2
(test (run '{{fun {x} x} {+ 1 {{fun {y} 2} 1}}} (mtSub)) (numV 6))

; run #3
(run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub)) ;; numV-n: contract violation ... [The interp is terminated with an error]