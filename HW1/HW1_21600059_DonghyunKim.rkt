#lang plai
;Problem 1:
;Solved by myself: Y
;Time taken: about 2 mins
;[contract] mile->km: number->number
;[purpose] To convert mile to km
;[tests] (test (mile->km 1)1.6)
;        (test (mile->km 2)3.2)
(define (mile->km mile)
  (* 1.6 mile))

(test (mile->km 1)1.6)
(test (mile->km 2)3.2)



;Problem 2:
;Solved by myself: Y
;Time taken: about 5 mins
;[contract] volume-cuboid: three numbers->number
;[purpose] To calculate voulme with three integer numbers
;[tests] (test (volume-cuboid 1 2 3)6)
;        (test (volume-cuboid 4 5 6)120)
(define (volume-cuboid width length height)
  (* height (* width length)))

(test (volume-cuboid 1 2 3)6)
(test (volume-cuboid 4 5 6)120)



;Problem 3:
;Solved by myself: Y
;Time taken: about 5 mins
;[contract] is-odd: number->boolean
;[purpose] To judge given integer number is odd or not
;[tests] (test (is-odd? 3)true)
;        (test (is-odd? 4)false)
(define (is-odd? number)
  (= (modulo number 2) 1))

(test (is-odd? 3)true)
(test (is-odd? 4)false)



;Problem 4:
;Solved by myself: Y
;Time taken: about 30 mins
;[contract] gcd: two numbers->number
;[purpose] To find the greatest common divisor of given two numbers
;[tests] (test (gcd 12 6)6)
;        (test (gcd 15 24)3)
;        (test (gcd 11 24)1)
;        (test (gcd 315 495)45)
;        (test (gcd 30 12)6)

(define (gcd num1 num2)
  (cond
    [(<= num1 num2) (gcd_recursive num1 num2 num1)]
    [else (gcd_recursive num1 num2 num2)]))
(define (gcd_recursive a b min)
  (cond
    [(= min 1) 1]
    [(and (= (modulo a min)0) (= (modulo b min)0)) min]
    [else (gcd_recursive a b (- min 1))]))

(test (gcd 12 6)6)
(test (gcd 15 24)3)
(test (gcd 11 24)1)
(test (gcd 315 495)45)
(test (gcd 30 12)6)



;Problem 5:
;Solved by myself: Y
;Time taken: about 30 mins
;[contract] lcm: two numbers->number
;[purpose] To find the least common multiple of given two numbers
;[tests] (test (lcm 12 6)12)
;        (test (lcm 4 6)12)
;        (test (lcm 12 20)60)
;        (test (lcm 7 9)63)

(define (lcm num1 num2)
  (cond
    [(<= num1 num2) (lcm_recursive num1 num2 num2)]
    [else (lcm_recursive num2 num1 num1)]))
(define (lcm_recursive a b interval)
  (cond
    [(= (modulo b a)0) b]
    [else (lcm_recursive a (+ b interval) interval)]))

(test (lcm 12 6)12)
(test (lcm 4 6)12)
(test (lcm 12 20)60)
(test (lcm 7 9)63)



;Problem 6,6-a:
;Solved by myself: Y
;Time taken: about 60 mins
;[contract] have-homework: COURSE->number
;[purpose] To show each COURSE's number of homework 
;[tests] (test (have-homework (ECE20016 2 4)) 4)
;(test (have-homework (ITP40001 0 7)) 7)
;(test (have-homework (ITP20005 2)) 2)

;Problem 6 define type
(define-type COURSE
  [ECE20016 (lab integer?)
            (homework integer?)]
  [ITP20005 (homework integer?)]
  [ITP40001 (projects integer?)
            (homework integer?)])


;Problem 6-a have-homework function
(define (have-homework course)
  (cond
    [(ECE20016? course) (ECE20016-homework course)]
    [(ITP20005? course) (ITP20005-homework course)]
    [else (ITP40001-homework course)]))

(test (have-homework (ECE20016 2 4)) 4)
(test (have-homework (ITP40001 0 7)) 7)
(test (have-homework (ITP20005 2)) 2)



;Problem 6-b:
;Solved by myself: Y
;Time taken: about 20 mins
;[contract] have-projects: COURSE->boolean
;[purpose] To show given course has more than two projects
;[tests] (test (have-projects (ITP40001 4 7)) true)
;(test (have-projects (ITP40001 2 7)) true)
;(test (have-projects (ITP40001 0 7)) false)
;(test (have-projects (ITP20005 7)) false)
;(test (have-projects (ECE20016 3 7)) false)

(define (have-projects course)
  (cond
    [(and (ITP40001? course) (<= 2 (ITP40001-projects course))) true]
    [else false]))

(test (have-projects (ITP40001 4 7)) true)
(test (have-projects (ITP40001 2 7)) true)
(test (have-projects (ITP40001 0 7)) false)
(test (have-projects (ITP20005 7)) false)
(test (have-projects (ECE20016 3 7)) false)



;Problem 7:
;Solved by myself: Y
;Time taken: about 60 mins
;[contract] name-pets: list->list
;[purpose] To make a name list of selected pet(dog cat pig)
;[tests](test (name-pets '(dog monkey pig cat)) '(happy () pinky smart))
;(test (name-pets '(pig dog cat)) '(pinky happy smart))
;(test (name-pets '(bird pig dog cat)) '(() pinky happy smart))

(define (name-pets lst)
  (cond
    [(empty? lst)  empty]
    [(symbol=? 'dog (first lst)) (append (list 'happy) (name-pets (rest lst)))]
    [(symbol=? 'cat (first lst)) (append (list 'smart) (name-pets (rest lst)))]
    [(symbol=? 'pig (first lst)) (append (list 'pinky) (name-pets (rest lst)))]
    [else (append '(()) (name-pets (rest lst)))]))

(test (name-pets '(dog monkey pig cat)) '(happy () pinky smart))
(test (name-pets '(pig dog cat)) '(pinky happy smart))
(test (name-pets '(bird pig dog cat)) '(() pinky happy smart))



;Problem 8:
;Solved by myself: Y
;Time taken: about 30 mins
;[contract] give-name: symbol, symbol, list->list
;[purpose] To make a name list which converted by given name
;[tests] (test (give-name 'cat 'tom (cons 'pig (cons 'bear (cons 'cat empty)))) '(pig bear tom))
;(test (give-name 'bear 'pooh (cons 'pig (cons 'bear (cons 'cat empty)))) '(pig pooh cat))
;(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty)))) '(pig cat pooh))
;(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear(cons 'bear empty))))) '(pig cat pooh pooh))

(define (give-name old new lst)
  (cond
    [(empty? lst)  empty]
    [(symbol=? old (first lst)) (append (list new) (give-name old new (rest lst)))]
    [else (append (list (first lst)) (give-name old new (rest lst)))]))

(test (give-name 'cat 'tom (cons 'pig (cons 'bear (cons 'cat empty)))) '(pig bear tom))
(test (give-name 'bear 'pooh (cons 'pig (cons 'bear (cons 'cat empty)))) '(pig pooh cat))
(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty)))) '(pig cat pooh))
(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear(cons 'bear empty))))) '(pig cat pooh pooh))
