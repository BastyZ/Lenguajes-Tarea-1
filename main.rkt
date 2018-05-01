#lang play
(require "machine.rkt")

;; Uncomment next line to print only failing tests.
(print-only-errors #t)

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
    [(list 'with (list id ': targ n) body) (app (fun id (parse-type targ) (parse body) #f) (parse n))]
    [(list left-arg right-arg) (app (parse left-arg) (parse right-arg))]
    ))

;; prettify :: <type> ->  <list>  (sintaxis concreta)
;;   Prints a type expresion on a legible way
(define (prettify type)
  (match type
    [#f #f]
    [(TNum) 'Num]
    [(TFun l r) {list (prettify l) '-> (prettify r)}] ))


;; PROBLEM 2

;; Class0424 thing :0
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
      ;; (+ a b) :: (TNum) x (TNum) -> (TNum)
      [(add (TNum) (TNum)) (TNum)]
      [(add a b) (if (and (equal? (typeof-env a env) (TNum)) (equal? (typeof-env b env) (TNum)))
                     (TNum)
                     (add (typeof-env a env) (typeof-env b env)))]
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
      [(app (fun id idtype arg #f) fun-arg)
       (if (equal? idtype (typeof-env fun-arg (extend-type-env id idtype env)))
           (typeof-env (fun id idtype arg (typeof-env arg (extend-type-env id idtype env))) (extend-type-env id idtype env))
           (error "Type error in expression app position 2: expected" (prettify idtype) 'found (prettify (typeof-env fun-arg (extend-type-env id idtype env)))))]
      [(app (fun id idtype arg argtype) fun-arg)
       (if (equal? idtype (typeof-env fun-arg (extend-type-env id idtype env)))
           (if (equal? (typeof-env arg (extend-type-env id idtype env)) argtype)
               (typeof-env (fun id idtype arg argtype) (extend-type-env id idtype env))
               (error "Type error in expression fun position 1: expected" (prettify argtype) 'found (prettify (typeof-env arg (extend-type-env id idtype env)))))
           (error "Type error in expression app position 2: expected" (prettify idtype) 'found (prettify (typeof-env fun-arg (extend-type-env id idtype env)))))]
      ))
  (typeof-env expr (mtTypeEnv))
  )

;; typecheck :: <type> -> <list>
;;   Hace exactamente lo mismo que typeof,
;;   pero lo entrega en sintaxis concreta
(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))


;; PROBLEM 3

;; Class0424 thing :0
#|-----------------------------
Environment abstract data type (brujin)
 
empty-env  :: Env
extend-env :: num id Env -> Env
lookup-env :: num Env -> type
 
representation BNF:
<env> ::= (mtEnv)
        | (aEnv <num> <id> <env>)
|#
(deftype BrujinEnv
  (mtBrujinEnv)
  (aBrujinEnv index val env))
 
(def empty-brujin-env  (mtBrujinEnv))
 
(define (extend-b-env value env)
  (define (suma env)
    (match env
      [(mtBrujinEnv) (mtBrujinEnv)]
      [(aBrujinEnv index val rest) (aBrujinEnv (add1 index) val (suma rest))]
      ))
  (aBrujinEnv 0 value (suma env)))

(define (lookup-brujin-env x env)
  (match env
    [(mtBrujinEnv) (error "Free identifier:" x)]
    [(aBrujinEnv index val rest)
     (if (symbol=? val x)
         index
         (lookup-brujin-env x rest))]))

;; deBrujin :: <Expr> -> <Expr>
;;   Given an expresion, replaces every id for a index of access
;;   and returns the modifies expresion
(define (deBruijn expr)
  (define (brujin-env expr env)
    (match expr
      [(num n) (num n)]
      [(id x) (acc (lookup-brujin-env x env))]
      [(add l r) (add (brujin-env l env) (brujin-env r env))]
      [(fun id targ body tbody) (fun-db (brujin-env body (extend-b-env id env)))] ;; TODO avanzar
      [(app l r) (app (brujin-env l env) (brujin-env r env))]
      ))
  (brujin-env expr (mtBrujinEnv)))

;; compile :: <Expr> -> List[V]
;;   Dada una expresion, retorna una lista de instrucciones para la maquina SECD
(define (compile expr)
  (define (compile-list expr lista)
    (match expr
      [(num n) (cons (INT-CONST n) lista)]
      [(acc n) (cons (ACCESS n) lista)]
      [(add l r) (compile-list r (compile-list l (cons (ADD) lista)))]
      [(fun-db body) (cons (CLOSURE (compile-list body (cons (RETURN) '()))) lista)]
      [(app l r) (compile-list r (compile-list l (cons (APPLY) lista)))]
      ))
    (compile-list expr '()))

;; typed-compile :: <s-expr> -> List[V]
;;   Genera el códico de maquina, verificando tipos, a partir de una expresion
(define (typed-compile s-expr)
  (begin
    (typecheck s-expr)
    (compile (deBruijn (parse s-expr)))
    ))



