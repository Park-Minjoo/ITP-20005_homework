#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

;[contract] parse: sexp->AE
;[purpose] to convert s-expressions into AEs in abstract syntax
;Time Taken: about 1 hour
;Solved by my self:Y

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ a b) (add (parse a) (parse b))]
    [(list '- a b) (sub (parse a) (parse b))]
    [else (error 'parse "bad syntax: ~a" sexp)]    
    ))


;; [tests]
(test (parse '3) (num 3))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{- 5 1}) (sub (num 5) (num 1)))
(test (parse '{+ {- {+ 1 3} 4} 6}) (add (sub (add (num 1) (num 3)) (num 4)) (num 6)))

(test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
(test/exn (parse '{- 5 1 2 3}) "parse: bad syntax: (- 5 1 2 3)")
(test/exn (parse '{* 5 1}) "parse: bad syntax: (* 5 1)")
(test/exn (parse '{* a 1}) "parse: bad syntax: (* a 1)")