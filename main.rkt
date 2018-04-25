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

(define (parse-type s-expr) (void))

(define (parse s-expr) (void))

(define (prettify type) (void))


;; PROBLEM 2

(define (typeof expr) (void))

(define (typecheck s-expr) (void))


;; PROBLEM 3

(define (deBruijn expr) (void))

(define (compile expr) (void))

(define (typed-compile s-expr) (void))