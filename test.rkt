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
(test/exn (typecheck '{+ x 5}) "Type error: No type for identifier: x")
(test (typecheck '{fun {x : Num} : Num 5}) '(Num -> Num))
(test (typecheck 20) 'Num)
(test (typecheck '{fun {x : Num} : Num {+ x x}}) '(Num -> Num))
(test/exn (typeof (parse '{{fun {x : Num} : {Num -> Num} 10} 5})) "Type error in expression fun position 1: expected (Num -> Num) found Num")

;; P 3.1
(test (extend-b-env 200 (extend-b-env 100 (mtBrujinEnv))) (aBrujinEnv 0 200 (aBrujinEnv 1 100 (mtBrujinEnv))))
(test/exn (deBruijn (add (num 1) (id 'x))) "Free identifier: x")
(test (deBruijn (fun 'x (TNum) (add (num 10) (num 1)) (TNum))) (fun-db (add (num 10) (num 1))))
(test (deBruijn (parse '{+ 1 {with {x : Num 1}
                           {with {y : Num 2}
                                 {+ x y}}}})) (add
                                               (num 1)
                                               (app
                                                (fun-db
                                                 (app (fun-db (add (acc 1) (acc 0))) (num 2)))
                                                (num 1))))
(test (deBruijn
       (parse '{{fun {x : Num} : Num
                     {+ x 10}} {+ 2 3}}))
      (app (fun-db (add (acc 0) (num 10))) (add (num 2) (num 3))))

;; P 3.2

(test (compile (deBruijn (parse '{{fun {x : Num} : Num
                                   {+ x 10}} {+ 2 3}})))
(list
 (INT-CONST 3)
 (INT-CONST 2)
 (ADD)
 (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN)))
 (APPLY)) )
(test/exn (compile (deBruijn (add (num 1) (id 'x)))) "Free identifier: x")
(test (compile (deBruijn (parse '{{fun {x : Num} : Num
                                   x} {+ 2 3}})))
(list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (ACCESS 0) (RETURN))) (APPLY)))
(test/exn (compile (deBruijn (parse '{with {y : Num 2}
             {+ x y}}))) "identifier: x")

;; P 3.3
(test/exn (typed-compile '{with {y : Num 2}
             {+ x y}}) "identifier: x")
(test (typed-compile '{{fun {x : Num} : Num
                                   x} {+ 2 3}})
      (list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (ACCESS 0) (RETURN))) (APPLY)))
(test/exn (typed-compile '(+ 1 x)) "identifier: x")
(test (typed-compile '{{fun {x : Num} : Num
                                   {+ x 10}} {+ 2 3}})
(list
 (INT-CONST 3)
 (INT-CONST 2)
 (ADD)
 (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN)))
 (APPLY)) )


