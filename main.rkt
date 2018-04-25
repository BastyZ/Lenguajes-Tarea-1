#lang play
(require "machine.rkt")

;; Uncomment next line to print only failing tests.
;; (print-only-errors #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            LANGUAGE DEFINITION                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {with {<id> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <type>} [: <type>] <s-expr>}
         | {<s-expr> <s-expr>}         
 
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body) ; Used in P3
  (acc n) ; Used in P3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            YOUR CODE GOES HERE                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: Remember to include the type signature, a brief description and tests
;; for every function defined.
;; (It's not mandatory to do this for functions defined INSIDE other functions)
;; Any function not following this will be counted as "not implemented" and
;; WILL NOT be evaluated


;; PROBLEM 1

;; parse-type :: <type> -> <type>
;;   Given a type expresion (signature form), returns type grammar
(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list arg -> ret) (TFun (parse-type arg) (parse-type ret))]
    [_ 'else "Parse error"] ))

;; parse :: <s-expr> -> <Expr>
;;   Given an sub-expresion, returns correspondent expresion
;;   Includes 'fun optional form
;;   Does not include P3 expresions
(define (parse s-expr) (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'fun (list id ': targ) ': tbody body) (fun id (parse-type targ) (parse body) (parse-type tbody))]
    [(list 'fun (list id ': targ) body) (fun id (parse-type targ) (parse body) #f)]
    [(list 'with (list id ': targ n) body) (app (fun id (parse-type targ) (parse body) #f) (parse n))]
    [(list fun-id arg-expr) (app fun-id arg-expr)] ))

(define (prettify type) (void))


;; PROBLEM 2

(define (typeof expr) (void))

(define (typecheck s-expr) (void))


;; PROBLEM 3

(define (deBruijn expr) (void))

(define (compile expr) (void))

(define (typed-compile s-expr) (void))