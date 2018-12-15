#lang plai-typed ; you need to install plai-typed package. Please see L27 slides to install it.

; (1) Solved by yourself?:
; (2) Time taken:

; type definition for abstract syntax tree of FAE
(define-type TFAE
    [bool    (b : boolean)]
    [num     (n : number)]
    [add     (lhs : TFAE) (rhs : TFAE)]
    [sub     (lhs : TFAE) (rhs : TFAE)]
    [id      (name : symbol)]
    [fun     (param : symbol) (type : TE) (body : TFAE)]
    [app     (ftn : TFAE) (arg : TFAE)])

(define-type TFAE-Value
  [boolV     (b : boolean)]
  [numV      (n : number)]
  [closureV  (param : symbol) (body : TFAE) (ds : DefrdSub)])

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))


(define-type DefrdSub
  [mtSub]
  [aSub (name : symbol) (value : TFAE-Value) (ds : DefrdSub)])

;lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i v saved) (if(symbol=? i name)
                                v
                                (lookup name saved))]))

; interp: FAE DefrdSub -> FAE-Value
(define (interp fae ds)
  (type-case TFAE fae
     [bool (b)     (boolV b)]
     [num (n)      (numV n)]
     [add (l r)    (num+ (interp l ds) (interp r ds))]
     [sub (l r)    (num- (interp l ds) (interp r ds))]
     [id  (s)     (lookup s ds)]
     [fun (p te b)  (closureV p b ds)]
     [app (f a)   (local [(define f-val (interp f ds))
                          (define a-val (interp a ds))]
                   (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val))))]))

(define-type TE
  [numTE]
  [boolTE]
  [arrowTE (arg : TE) (result : TE)])

(define-type Type
  [numT]
  [boolT]
  [arrowT (arg : Type) (reult : Type)])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol) (type : Type) (rest : TypeEnv)])

; (3) parse-type: 
; (4) purpose: 
(define (parse-type te)
      ...) ; TODO: implement this function

;type-error: 
(define (type-error tfae terr)
  (error  'type-error "bad type"))

;get-type: symbol TypeEnv -> Type
(define (get-type name env)
  (type-case TypeEnv env
    [mtEnv ()           (error 'get-type "no type")]
    [aBind  (i t rest) (if(symbol=? i name)
                                t
                                (get-type name rest))]))

; (5) typecheck:
; (6) purpose: 
(define typecheck : (TFAE TypeEnv -> Type)
  (lambda (tfae env)
    (type-case TFAE tfae
      [bool (b)  (boolT)]
      [num (n)   (numT)]
      [add (l r) (type-case Type (typecheck l env)
                    [numT() (type-case Type (typecheck r env)
                               [numT () (numT)]
                               [else (type-error r "num")])]
                    [else (type-error l "num")])]
      [sub (l r) (type-case Type (typecheck l env)
                    [numT() (type-case Type (typecheck r env)
                               [numT () (numT)]
                               [else (type-error r "num")])]
                    [else (type-error l "num")])]
      [id  (name) (get-type name env)]
      [fun (name te body) (local [(define param-type (parse-type te))]
                             (arrowT param-type (typecheck body (aBind name param-type
                                            env))))]
      [app (fn arg) (type-case Type (typecheck fn env)
                      [arrowT (param-type result-type)
                              (if (equal? param-type
                                          (typecheck arg env))
                                  result-type
                                  (type-error arg (to-string param-type)))]
                      [else (type-error fn "function")])])))

; (7) eval : 
; (8) purpose: 
(define eval : (TFAE -> TFAE-Value)
  (lambda (tfae)
    (begin
      (try (typecheck tfae (mtEnv))
           (lambda() (error 'type-error "typecheck")))
      (interp tfae (mtSub)))))

; test cases
(test (eval (num 3)) (numV 3))
(test/exn (eval (add (num 1) (bool false))) "type-error: typecheck")
(test (eval (sub (num 1) (num 3))) (numV -2))
(test (eval (add (num 9) (sub (num 1) (num 3)))) (numV 7))
(test/exn (eval (add (num 9) (sub (fun 'x (numTE) (id 'x)) (num 3)))) "type-error: typecheck")

(test (eval (fun 'x (numTE) (id 'x))) (closureV 'x (id 'x) (mtSub)))

(test/exn (eval (app (fun 'x (boolTE) (id 'x)) (num 3))) "type-error: typecheck")
(test (eval (app (fun 'x (boolTE) (id 'x)) (bool false))) (boolV #f))

(test/exn (eval (add (fun 'x (numTE) (id 'x)) (num 3))) "type-error: typecheck") ; lhs must be num type always.
(test/exn (eval (add (num 5) (fun 'z (numTE) (id 'z)))) "type-error: typecheck") ; rhs must be num type always.
(test/exn (eval (app (num 7) (num 5))) "type-error: typecheck") ; This is not a function call actually
(test/exn (eval (app (id 'x) (num 5))) "type-error: typecheck") ; (9) Why is this type-error? (                                                )
(test/exn (eval (app (fun 'x (numTE) (id 'x)) (fun 'x (numTE) (id 'x)))) "type-error: typecheck"); (10) Why is this type-error?:(                )