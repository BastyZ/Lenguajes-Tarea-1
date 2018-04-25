#lang play
(require "main.rkt")
(require "machine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            YOUR TESTS GO HERE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; P 1.1
(test (parse-type '((Num -> Num) -> Num)) (TFun (TFun (TNum) (TNum)) (TNum)))
(test (parse-type '{Num ->  }) "Parse error" )
(test (parse-type 'Num) (TNum) )
(test (parse-type '(Num)) "Parse error")

;; P 1.2
(test (parse '{fun {x : Num} : Num {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)) )
(test (parse '{with {y : Num 2} {+ x y}}) (app (fun 'y (TNum) (add (id 'x) (id 'y)) #f) (num 2)) )
(test (parse 2) (num 2) )
(test (parse '{fun {x : Num} {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) #f) )