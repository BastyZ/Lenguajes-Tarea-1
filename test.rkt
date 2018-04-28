#lang play
(require "main.rkt")
(require "machine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            YOUR TESTS GO HERE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; P 1.1
(test (parse-type '((Num -> Num) -> Num)) (TFun (TFun (TNum) (TNum)) (TNum)))
(test/exn (parse-type '{Num ->  }) "Parse error" )
(test (parse-type 'Num) (TNum) )
(test/exn (parse-type '(Num)) "Parse error")

;; P 1.2
(test (parse '{fun {x : Num} : Num {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)) )
(test (parse '{with {y : Num 2} {+ x y}}) (app (fun 'y (TNum) (add (id 'x) (id 'y)) #f) (num 2)) )
(test (parse 2) (num 2) )
(test (parse '{fun {x : Num} {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) #f) )

;; P 1.3
(test (prettify (TNum)) 'Num)
(test (prettify (TFun (TNum) (TNum))) '(Num -> Num) )
(test (prettify (TFun (TNum) (TFun (TNum) (TNum)))) '(Num -> (Num -> Num)) )
(test (prettify (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum)))) '((Num -> Num) -> (Num -> Num)))

;; P 2.1
(test (typeof (parse 5)) (TNum))
(test/exn (typeof (parse 'x)) "Type error: No type for identifier: x")
(test (typeof (parse '{fun {x : Num} : Num 5})) (TFun (TNum) (TNum)))
(test (typeof (parse '{fun {x : Num} x})) (TFun (TNum) (TNum)))
(test/exn (typeof (parse '{fun {x : Num} : {Num -> Num} 10})) "Type error in expression fun position 1: expected (Num -> Num) found Num")
(test/exn (typeof (parse 'y)) "Type error: No type for identifier: y")
(test/exn (typeof (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}})) "Type error in expression app position 2: expected Num found (Num -> Num)" )

;; P 2.2
(test/exn (typecheck (parse '{+ x 5})) "Type error: No type for identifier: x")
(test (typecheck (parse '{fun {x : Num} : Num 5})) '(Num -> Num))
(test (typecheck (parse 20)) 'Num)
(test (typecheck (parse '{fun {x : Num} : Num {+ x x}})) '(Num -> Num))
(test/exn (typeof (parse '{{fun {x : Num} : {Num -> Num} 10} 5})) "Type error in expression fun position 1: expected (Num -> Num) found Num")

