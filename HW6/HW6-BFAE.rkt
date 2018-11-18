#lang plai

; (1) Solve by myself: ...
; (2) Time taken: ...

; type definition for abstract syntax tree of BFAE
(define-type BFAE
    [num    (n number?)]
    [add     (lhs BFAE?) (rhs BFAE?)]
    [sub     (lhs BFAE?) (rhs BFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body BFAE?)]
    [newbox  (v BFAE?)]
    [setbox  (bn BFAE?) (v BFAE?)]
    [openbox  (v BFAE?)]
    [seqn  (ex1 BFAE?) (ex2 BFAE?)]
    [app     (ftn BFAE?) (arg BFAE?)]
  )
  
        
; parse: sexp -> BFAE
; (3) purpose: ...
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'newbox v)           (newbox (parse v))]
        [(list 'setbox i v)         (setbox (parse i) (parse v))]
        [(list 'openbox i)          (openbox (parse i))]
        [(list 'seqn ex1 ex2)       (seqn (parse ex1) (parse ex2))]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

(define-type BFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV      (address integer?)])

; (4) num-op: ...
; (5) purpose: ...
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto   (address integer?) (value BFAE-Value?)
          (rest Store?)])

; lookup: symbol DefrdSub -> address
; (6) purpose: ...
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i adr saved) (if(symbol=? i name)
                                adr
                                (lookup name saved))]))

; (7) store-lookup: ...
; (8) purpose: ...
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto ()           (error 'store-lookup "No value at address")]
    [aSto  (location value rest-store)
                 (if(= location address)
                     value
                     (store-lookup address rest-store))]))

; (9) malloc: ...
; (10) purpose: ...
(define (malloc st)
  (+ 1 (max-address st)))

; (11) max-address: ...
; (12) purpose: ...
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

(define-type Value*Store
  [v*s (value BFAE-Value?) (store Store?)])

; (13) interp-two: ...
; (14) purpose: ...

; (15) interp: ...
; (16) purpose: ...
(define (interp bfae ds st)
  (type-case BFAE bfae
     [num (n)      0]
     [add (l r)    (type-case Value*Store (interp l ds st)
                       [v*s (l-value l-store)
                            (type-case Value*Store (interp r ds l-store)
                                [v*s (r-value r-store)
                                     (v*s (num+ l-value r-value)
                                          r-store)])])]
     [sub (l r)    (type-case Value*Store (interp l ds st)
                       [v*s (l-value l-store)
                            (type-case Value*Store (interp r ds l-store)
                                [v*s (r-value r-store)
                                     (v*s (num- l-value r-value)
                                          r-store)])])]
     [id  (s)    0]
     [fun (p b)  (v*s (closureV p b ds) st)]
     [app (f a)  (type-case Value*Store (interp f ds st)
                     [v*s (f-value f-store)
                          (type-case Value*Store (interp a ds f-store)
                              [v*s (a-value a-store)
                                   (local ([define new-address (malloc a-store)])
                                     (interp (closureV-body f-value)
                                             (aSub (closureV-param f-value)
                                                   new-address
                                                   (closureV-ds f-value))
                                             (aSto new-address
                                                   a-value
                                                   a-store)))])])]
     [newbox (val) 0]
     [setbox (bx-expr val-expr)
                               (type-case Value*Store (interp bx-expr ds st)
                                 [v*s (bx-val st2)
                                      (type-case Value*Store (interp val-expr ds st2)
                                        [v*s (val st3)
                                             (v*s val
                                                  (aSto (boxV-address bx-val)
                                                        val
                                                        st3))])])]
     [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val)
                                       st1)
                         st1)])]
     [seqn (a b) (type-case Value*Store (interp a ds st)
                  [v*s (a-value a-store)
                       (interp b ds a-store)])]
    ))

; (17) run:
; (18) purpose: ...
(define (run sexp ds st)
     (interp (parse sexp) ds st))

(test (run '7 (mtSub) (mtSto)) (v*s (numV 7) (mtSto)))
(test (run '{+ 2 3} (mtSub) (mtSto)) (v*s (numV 5) (mtSto)))
(test (run '{with {a 7} a} (mtSub) (mtSto)) (v*s (numV 7) (aSto 1 (numV 7) (mtSto))))
(test (run '{with {v 7} {{fun {a} a} v}} (mtSub) (mtSto)) (v*s (numV 7) (aSto 2 (numV 7) (aSto 1 (numV 7) (mtSto)))))
(test (run '{newbox 7} (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (numV 7) (mtSto))))
(test (run '{+ {with {b {newbox 10}}
                        {seqn {setbox b 7}
                              {openbox b}}} {with {b {newbox 10}}
                        {seqn {setbox b 5}
                              {openbox b}}}} (mtSub) (mtSto))
      (v*s (numV 12) (aSto 3 (numV 5) (aSto 4 (boxV 3) (aSto 3 (numV 10) (aSto 1 (numV 7) (aSto 2 (boxV 1) (aSto 1 (numV 10) (mtSto)))))))))
(test (run '{+ {with {b {newbox 3}} {seqn
                              {setbox b 10}
                              {openbox b}}} 2} (mtSub) (mtSto))
      (v*s (numV 12) (aSto 1 (numV 10) (aSto 2 (boxV 1) (aSto 1 (numV 3) (mtSto))))))
(test (run '{with {b {newbox 7}} b} (mtSub) (mtSto)) (v*s (boxV 1) (aSto 2 (boxV 1) (aSto 1 (numV 7) (mtSto)))))
(test (run '{with {nb {newbox 7}} {seqn {{fun {b} {setbox b 10}} nb} {openbox nb}}}  (mtSub) (mtSto))
      (v*s (numV 10) (aSto 1 (numV 10) (aSto 3 (boxV 1) (aSto 2 (boxV 1) (aSto 1 (numV 7) (mtSto)))))))
(test (run '{with {q {newbox 10}}
    {setbox {seqn {setbox q 12} q}
                   {openbox q}}}  (mtSub) (mtSto)) (v*s (numV 12) (aSto 1 (numV 12) (aSto 1 (numV 12) (aSto 2 (boxV 1) (aSto 1 (numV 10) (mtSto)))))))
(test (run '{+ 2 {setbox {newbox 5} 7}} (mtSub) (mtSto))
      (v*s (numV 9) (aSto 1 (numV 7) (aSto 1 (numV 5) (mtSto)))))
(test (run '{+ 2 {with {b {newbox 5}} {setbox b 7}}} (mtSub) (mtSto)) (v*s (numV 9) (aSto 1 (numV 7) (aSto 2 (boxV 1) (aSto 1 (numV 5) (mtSto))))))
(test (run '{+ 2 {with {b {newbox 5}} {seqn {setbox b 7} {openbox b}}}} (mtSub) (mtSto)) (v*s (numV 9) (aSto 1 (numV 7) (aSto 2 (boxV 1) (aSto 1 (numV 5) (mtSto))))))