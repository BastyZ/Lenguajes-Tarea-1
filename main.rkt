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
    [_ 'else (error "Parse error")] ))

;; parse :: <s-expr> -> <Expr>
;;   Given an sub-expresion, returns correspondent expresion
;;   Includes 'fun optional form
;;   Does not include P3 expresions
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'fun (list id ': targ) ': tbody body) (fun id (parse-type targ) (parse body) (parse-type tbody))]
    [(list 'fun (list id ': targ) body) (fun id (parse-type targ) (parse body) #f)]
    [(list 'with (list id ': targ n) body) (app (fun id (parse-type targ) (parse body) #f) (parse n))] ))

;; prettify :: <type> ->  <list>
;;   Prints a type expresion on a legible way
(define (prettify type)
  (match type
    [(TNum) 'Num]
    [(TFun l r) {list (prettify l) '-> (prettify r)}] ))


;; PROBLEM 2

;; Auxiliar thing
#|-----------------------------
Environment abstract data type
 
empty-env  :: Env
extend-env :: Sym type Env -> Env
lookup-env :: Sym Env -> type
 
representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <type> <env>)
|#
(deftype TypeEnv
  (mtTypeEnv)
  (aTypeEnv id type env))
 
(def empty-type-env  (mtTypeEnv))
 
(def extend-type-env aTypeEnv)

(define (lookup-type-env x env)
  (match env
    [(mtTypeEnv) (error "Type error: No type for identifier:" x)]
    [(aTypeEnv id type rest)
     (if (symbol=? id x)
         type
         (lookup-type-env x rest))]))

;; typeof :: <Expr> -> <type>
;;   Recibe lo que le entrega parse para retornar el tipo
(define (typeof expr)
  (define (typeof-env expr env)
    (match expr
      [(num n) (TNum)]
      [(id  x) (lookup-type-env x env)]
      [(fun id idtype arg #f)
       (typeof-env
        (fun id idtype arg (typeof-env arg (extend-type-env id idtype env)))
        (extend-type-env (parse id) idtype env))]
      [(fun id idtype arg argtype)
       (begin
         (define new-env (extend-type-env id idtype env))
         (if (equal? (typeof-env arg new-env) argtype)
           (TFun (typeof-env (parse id) new-env) argtype)
           (error "Type error in expression fun position 1: expected" (prettify argtype) 'found (prettify (typeof-env arg new-env)))))]
        ))
  (typeof-env expr (mtTypeEnv))
  )

(define (typecheck s-expr) (void))


;; PROBLEM 3

(define (deBruijn expr) (void))

(define (compile expr) (void))

(define (typed-compile s-expr) (void))



