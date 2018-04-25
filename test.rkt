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


