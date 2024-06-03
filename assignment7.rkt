; parser and type-checker implemented, locals and rec updated, no interp

#lang typed/racket
(require typed/rackunit)

; define type for ExprC
(define-type ExprC (U NumC IdC StrC IfC LambC RecC AppC))

; structs for ExprC types
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct StrC ([val : String]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LambC ([params : (Listof Symbol)]
               [types : (Listof Type)]
               [returns : (Option Type)]
               [body : ExprC])
  #:transparent)
(struct RecC ([name : Symbol]
              [func : LambC]
              [call : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)


; takes in an Sexp and converts it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? sym) (if (member sym '(if lamb locals locals-rec -> : =)) 
                         (error "ZODE: Invalid use of reserved keyword as an identifier: ~e" sym)
                         (IdC sym))] 
    [(? string? str) (StrC str)]
    [(cons f r) (match f
                  ['if (parse-if r)]
                  ['locals (parse-locals r)]
                  ['locals-rec (parse-rec r)]
                  ['lamb (parse-lamb r)]
                  [other (parse-app s)])]
    [other (error 'parse "ZODE: Invalid concrete syntax, cannot parse: ~e" s)]))


; takes in an Sexp and converts it into an IfC
(define (parse-if [s : Sexp]) : IfC
  (match s
    [`(: ,test : ,then : ,else) (IfC (parse test)
                                     (parse then)
                                     (parse else))]
    [other (error 'parse-if "ZODE: Invalid use of 'if': ~e" s)]))


; takes in an Sexp and converts it into an AppC representation of a locals
(define (parse-locals [s : Sexp]) : AppC
  (match s
    [(cons ': (? list? r)) (let ([clauses (parse-clauses (split-clauses (cast (drop-right r 1)
                                                                              (Listof Sexp))))])
                             (if (check-duplicates (first clauses))
                                 (error 'parse-locals
                                        "ZODE: Cannot have two parameters with the same name: (first clauses)")
                                 (AppC (LambC (cast (first clauses)
                                                    (Listof Symbol))
                                              (cast (second clauses)
                                                    (Listof Type))
                                              #f
                                              (parse (last r)))
                                       (cast (third clauses)
                                             (Listof ExprC)))))]))

; takes in an Sexp and converts it into a list of parameters (Symbols) and a list of their values (ExprC)
(define (parse-clauses [s : (Listof Sexp)]) :  (Listof (U (Listof Type)(Listof Symbol) (Listof ExprC)))
  (match s
    [(list (list tys (? symbol? ids) '= vals ':) ...) (list (cast ids (Listof Symbol))
                                                            (for/list : (Listof Type)
                                                              ([ty tys])
                                                              (parse-type (cast ty Sexp)))
                                                            (for/list : (Listof ExprC)
                                                              ([val vals])
                                                              (parse (cast val Sexp))))]
    [other (error 'parse-clauses "ZODE: Clauses not formatted correctly: ~e" s)]))

; takes in a list of clauses and splits them into their own lists
(define (split-clauses [s : (Listof Sexp)]) : (Listof (Listof Sexp))
  (match (length s)
    [5 (list s)]
    [(? real? n) (if (> n 5)
                     (cons (take s 5) (split-clauses (drop s 5)))
                     (error 'split-clauses "ZODE: Incorrect number of parameters in clauses: ~e" s))]))


; takes in an Sexp and converts it into a LambC
(define (parse-lamb [s : Sexp]) : LambC
  (match s
    [(list ': [list types (? symbol? params)] ... '-> ret ': body)
     (if (check-duplicates params)
         (error 'parse-lamb "ZODE: Same name is used for multiple parameters in: ~e" s)
         (LambC (cast params (Listof Symbol))
                (for/list : (Listof Type)
                  ([type types])
                  (parse-type (cast type Sexp)))
                (parse-type ret)
                (parse body)))]
    [other (error 'parse-lamb "ZODE: Invalid use of 'lamb': ~e" s)]))


; takes in an Sexp and converts it into an AppC
(define (parse-app [s : Sexp]) : AppC
  (match s
    [(list func args ...) (AppC (parse func)
                                (for/list  : (Listof ExprC)
                                  ([arg args])
                                  (parse (cast arg Sexp))))]))


; data definition for types
(define-type Type (U 'num 'str 'bool LamT))
(struct LamT ([in : (Listof Type)] [out : Type]) #:transparent)

(define (parse-type [s : Sexp]) : Type
  (match s
    ['num 'num]
    ['str 'str]
    ['bool 'bool]
    [(list args ... '-> ret) (LamT (for/list : (Listof Type)
                                               ([arg args])
                                               (parse-type (cast arg Sexp)))
                                   (parse-type ret))]
    [other (error 'parse-type "ZODE: Not a valid type: ~e" s)]))


; takes in an ExprC and a type environment and returns the associated type
(define (type-check [e : ExprC] [tenv : tEnv]) : Type
  (match e
    [(NumC n) 'num]
    [(IdC n) (lookup-type n tenv)]
    [(StrC s) 'str]
    [(IfC test then else)
     (let ([test-t (type-check test tenv)]
           [then-t (type-check then tenv)]
           [else-t (type-check else tenv)])
       (cond
         [(not (equal? test-t 'bool))
          (error 'type-check "ZODE: 'test' statement must be a bool in 'if', got ~e" test-t)]
         [(equal? then-t else-t)
          then-t]
         [else (error 'type-check
                      "ZODE: 'then' and 'else' statements must be of the same type in 'if', got: ~e and ~e"
                      then-t else-t)])
       #;(if (equal? then-t else-t)
             then-t
             (error 'type-check
                    "ZODE: 'then' and 'else' statements must be of the same type in 'if', got: ~e and ~e"
                    then-t else-t)))]
    [(LambC params types returns body) (let ([body-t (type-check body
                                                                 (extend-ty-env params types tenv))])
                                         (if (member returns (list #f body-t))
                                             (LamT types body-t)
                                             (error 'type-check "ZODE: lambda type mismatch: ~e" e)))]
    [(RecC id (LambC params types returns body) call)
     (let ([new-tenv (extend-ty-env (list id)
                                    (list (LamT types (cast returns Type)))
                                    tenv)])
       (if (equal? returns
                   (type-check body
                               (extend-ty-env params
                                              types
                                              new-tenv)))
           (type-check call new-tenv)
           (error 'type-check "ZODE: lambda in locals-rec type mismatch: ~e" e)))]
    [(AppC f args) (let ([ft (type-check f tenv)]
                         [at (for/list : (Listof Type)
                               ([arg args])
                               (type-check arg tenv))])
                     (match ft
                       [(LamT in out)
                        (if (equal? in at)
                            out
                            (error 'type-check "ZODE: Arg types do not match lambda expected types: ~e and ~e" in at))]
                       [other (error 'type-check
                                     "ZODE: Expected first argument of application to be of type LamT, got: ~e"
                                     e)]))]))


; takes in an Sexp and converts it into a RecC
(define (parse-rec [s : Sexp]) : RecC
  (match s
    [`(: ,(? symbol? id) = ,(cons key lam) : ,call) (RecC id
                                               (parse-lamb lam)
                                               (parse call))]
    [other (error 'parse-rec "ZODE: Invalid use of 'locals-rec': ~e" s)]))


; define types for environments
(struct Bind ([name : Symbol] [val : Value]) #:transparent)
(struct tBind ([name : Symbol] [val : Type]) #:transparent)
(define-type tEnv (Listof tBind))
(define-type Env (Listof Bind))

; define Value type
(define-type Value (U Real String Boolean CloV PrimOpV))
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimOpV ([proc : ((Listof Value) -> Value)] [args : Integer]) #:transparent)

; define top environments
(define top-env
  (list
   (Bind '+ (PrimOpV (lambda (args)
                       (let ([arg1 (cast (car args) Real)] [arg2 (cast (cadr args) Real)])
                         (+ arg1 arg2))) 
                     2))
   (Bind '- (PrimOpV (lambda (args)
                       (let ([arg1 (cast (car args) Real)] [arg2 (cast (cadr args) Real)])
                         (- arg1 arg2)))
                     2))
   (Bind '* (PrimOpV (lambda (args)
                       (let ([arg1 (cast (car args) Real)] [arg2 (cast (cadr args) Real)])
                         (* arg1 arg2)))
                     2))
   (Bind '/ (PrimOpV (lambda (args)
                       (let ([arg1 (cast (car args) Real)] [arg2 (cast (cadr args) Real)])
                         (if (= arg2 0)
                             (error "ZODE: Division by zero.")
                             (/ arg1 arg2))))
                     2))
   (Bind '<= (PrimOpV (lambda (args)
                        (let ([arg1 (cast (car args) Real)] [arg2 (cast (cadr args) Real)])
                          (<= arg1 arg2)))
                      2))
   (Bind 'num-eq? (PrimOpV (lambda (args)
                            (let ([arg1 (car args)] [arg2 (cadr args)])
                              (equal? arg1 arg2)))
                          2))
   (Bind 'str-eq? (PrimOpV (lambda (args)
                            (let ([arg1 (car args)] [arg2 (cadr args)])
                              (equal? arg1 arg2)))
                          2))
   (Bind 'substring (PrimOpV (lambda (args)
                               (let ([arg1 (cast (car args) String)]
                                     [arg2 (cast (cadr args) Integer)]
                                     [arg3 (cast (caddr args) Integer)])
                                 (if (or (> arg2 arg3)
                                         (< arg2 0)
                                         (>= arg2 (string-length arg1))
                                         (>= arg3 (string-length arg1)))
                                     (error 'substring "ZODE: Array index out of bounds: ~e to ~e" arg2 arg3)
                                     (substring arg1 arg2 arg3))))
                             3))
   (Bind 'true true)
   (Bind 'false false)))

(define base-tenv
  (list
   (tBind '+ (LamT '(num num) 'num))
   (tBind '- (LamT '(num num) 'num))
   (tBind '* (LamT '(num num) 'num))
   (tBind '/ (LamT '(num num) 'num))
   (tBind '<= (LamT '(num num) 'bool))
   (tBind 'num-eq? (LamT '(num num) 'bool))
   (tBind 'str-eq? (LamT '(str str) 'bool))
   (tBind 'substring (LamT '(str num num) 'str))
   (tBind 'true 'bool)
   (tBind 'false 'bool)))


; takes in a list of parameters and a list of arguments and extends an environment by binding the corresponding values
(define (extend-env [params : (Listof Symbol)] [args : (Listof ExprC)] [env : Env]) : Env
  (match* (params args)
    [('() '()) env]
    [((cons first-param rest-param) (cons first-arg rest-arg)) (cons (Bind first-param
                                                                           (interp first-arg env))
                                                                     (extend-env rest-param
                                                                                 rest-arg env))]
    [(some other) (error 'extend-env "ZODE: Incorrect number of arguments, expected ~e, got ~e" params args)]))


; takes in a list of parameters and a list of Types and extends an environment by binding the corresponding Types
(define (extend-ty-env [params : (Listof Symbol)] [types : (Listof Type)] [tenv : tEnv]) : tEnv
  (match* (params types)
    [('() '()) tenv]
    [((cons first-param rest-param) (cons first-type rest-type)) (cons (tBind first-param first-type)
                                                                     (extend-ty-env rest-param
                                                                                 rest-type tenv))]
    [(some other) (error 'extend-ty-env "ZODE: Incorrect number of types, expected ~e, got ~e" params types)]))


; takes in a Symbol and returns the corresponding value in the environment if it exists
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "ZODE: Variable name not found: ~e" for)]
    [(cons f r) (if (symbol=? for (Bind-name f))
             (Bind-val f)
             (lookup for r))]))


; takes in a Symbol and returns the corresponding Type in the environment if it exists
(define (lookup-type [for : Symbol] [tenv : tEnv]) : Type
  (match tenv
    ['() (error 'lookup-type "ZODE: Variable name not found: ~e" for)]
    [(cons f r) (if (symbol=? for (tBind-name f))
             (tBind-val f)
             (lookup-type for r))]))


; Adjust the interp function to handle expressions directly without pre-evaluating arguments for closures
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(NumC n) n]
    [(IdC sym) (lookup sym env)]
    [(StrC str) str]
    [(IfC test then else) 
     (let ([test-result : Value (interp test env)])
       (if (and (boolean? test-result) test-result)
           (interp then env)
           (interp else env)))]
    [(LambC params types returns body) (CloV params body env)]
    [(AppC func args) 
     (let ([f : Value (interp func env)])
       (match f
         [(CloV params body closure-env)
          (interp body (extend-env params args closure-env))]
         [(PrimOpV proc arity)
              (let ([evaluated-args : (Listof Value) (evaluate-arguments args env)])
                (proc evaluated-args))]
         [other (error 'interp "ZODE: Cannot apply as a function: ~e" f)]))]))

; Helper function to evaluate arguments for primops, since extend-env handles unevaluated ExprC for closures
(define (evaluate-arguments [args : (Listof ExprC)] [env : Env]) : (Listof Value)
  (for/list ([arg : ExprC (in-list args)])
    (interp arg env)))


; takes in a ZODE4 value and converts it into a string
(define (serialize [v : Value]) : String
  (match v
    [(or (? real?) (? string?)) (~v v)]
    [(? boolean?) (cond
                    [(boolean=? #t v) "true"]
                    [else "false"])]
    [(CloV _ _ _) "#<procedure>"]
    #;[(PrimOpV _ _) "#<primop>"]))


; takes in an Sexp, parses it into an AST, type checks the AST, then interprets it into a value
(define (top-interp [s : Sexp]) : String
  (let ([ast (parse s)])
    (type-check ast base-tenv)
    (serialize (interp  ast top-env))))




; parse tests
(check-equal? (parse -2.2) (NumC -2.2))
(check-equal? (parse 'hi) (IdC 'hi))
(check-equal? (parse "string") (StrC "string"))
(check-exn #rx"ZODE: Invalid use of reserved keyword as an identifier" (λ () (parse 'if)))
(check-exn #rx"ZODE: Invalid concrete syntax, cannot parse" (λ () (parse #f)))

; parse-if tests
(check-equal? (parse '{if : 0 : b : "hi"}) (IfC (NumC 0) (IdC 'b) (StrC "hi")))
(check-exn #rx"ZODE: Invalid use of 'if'" (λ () (parse  '{if : not : enough})))

; parse-locals tests
(check-equal? (parse '{locals : str var = "var"
                              : num pi = 3.14
                              : 0})
              (AppC (LambC '(var pi)
                           '(str num)
                           #f
                           (NumC 0))
                    (list (StrC "var")
                          (NumC 3.14))))
(check-exn #rx"ZODE: Incorrect number of parameters in clauses" (λ () (parse '{locals : "hi" : 'm})))
(check-exn #rx"ZODE: Clauses not formatted correctly" (λ () (parse '{locals : 1 2 3 4 : 'no})))
(check-exn #rx"ZODE: Cannot have two parameters with the same name"
           (λ () (parse '{locals : num a = 0 : str a = "zero" : 'no})))

; parse-clauses tests
(check-equal? (parse-clauses '{{num x = 5 :}
                               {str y = "z" :}})
              (list '(x y)
                    '(num str)
                    (list (NumC 5)
                          (StrC "z"))))
(check-equal? (parse-clauses '{{num x = 5 :}}) (list '(x) '(num) (list (NumC 5))))

; split-clauses tests
(check-equal? (split-clauses '{num x = 0 : num y = 1 : num z = 2 :})
              '((num x = 0 :) (num y = 1 :) (num z = 2 :)))

; parse-lamb tests
(check-equal? (parse '{lamb : -> bool : 0}) (LambC '() '() 'bool (NumC 0)))
(check-equal? (parse '{lamb : [str a] [bool b] [num c] -> num : "hello"})
              (LambC '(a b c) '(str bool num) 'num (StrC "hello")))
(check-exn #rx"ZODE: Invalid use of 'lamb'" (λ () (parse '{lamb fail})))
(check-exn #rx"ZODE: Same name is used for multiple parameters"
           (λ () (parse '{lamb : [bool same] ['str same] -> num : u})))

; parse-app tests
(check-equal? (parse '{{lamb : [num y] -> num : 1} 0}) (AppC (LambC '(y) '(num) 'num (NumC 1)) (list (NumC 0))))
(check-equal? (parse '{{lamb : [str z] -> str : "test"}}) (AppC (LambC '(z) '(str) 'str (StrC "test")) '()))

; interp tests
(check-equal? (interp (IfC (IdC 'true)
                           (AppC (LambC '(x y)
                                        '(num num)
                                        'num
                                        (NumC 5))
                                 (list (NumC 0)
                                       (StrC "dummy")))
                           (NumC 10)) top-env)
              5)
(check-exn #rx"ZODE: Cannot apply as a function" (λ () (interp (IfC (IdC 'false)
                                                                    (IdC 'bye)
                                                                   (AppC (NumC pi) '()))
                                                               (list (Bind 'false #f)))))
                                        
; lookup tests
(check-equal? (lookup 'here (list (Bind 'here #t) (Bind 'nothere "nope"))) #t)
(check-exn #rx"ZODE: Variable name not found" (λ () (lookup 'dne (list (Bind 'lock 10)))))

; lookup-type tests
(check-equal? (lookup-type 'here (list (tBind 'here 'num) (tBind 'nothere 'str))) 'num)
(check-exn #rx"ZODE: Variable name not found" (λ () (lookup-type 'dne (list (tBind 'lock 'bool)))))

; extend-env tests
(check-equal? (extend-env '(a b c) (list (NumC 1) (NumC 2) (NumC 3)) '()) (list (Bind 'a 1) (Bind 'b 2) (Bind 'c 3)))
(check-exn #rx"Incorrect number of arguments" (λ () (extend-env '(x) '() '())))

; extend-ty-env tests
(check-equal? (extend-ty-env '(a b c)
                             '(num bool bool)
                             (list (tBind 'x 'str)))
              (list (tBind 'a 'num)
                    (tBind 'b 'bool)
                    (tBind 'c 'bool)
                    (tBind 'x 'str)))
(check-exn #rx"ZODE: Incorrect number of types" (λ () (extend-ty-env '(x) '() '())))

; serialize test
#;(check-equal? (serialize (PrimOpV (λ (args) ) 3) "#<primop>"))

; top-interp tests
(check-equal? (top-interp '{locals : num x = 5
                              : {+ x 2}}) "7")
(check-equal? (top-interp '{locals : num x = 5
                                   : "hi"}) "\"hi\"")
(check-equal? (top-interp '{locals : num x = 5
                                   : true}) "true")
(check-equal? (top-interp '{locals : num x = 5
                                   : false}) "false")
(check-equal? (top-interp '{locals : num x = 5
                                   : (lamb : -> num : 0)}) "#<procedure>")
(check-equal? (top-interp '{locals : num x = 0
                                   : {if : {<= {+ 1 2} {- 4 3}}
                                         : -1.2
                                         : {if : true
                                               : {* {/ 2 2} 1}
                                               : 0}}}) "1")
(check-equal? (top-interp '{num-eq? 0 1}) "false")
(check-equal? (top-interp '(str-eq? "hi" "hi")) "true")
(check-equal? (top-interp '{substring "hello" 3 4}) "\"l\"")
(check-exn #rx"ZODE: Array index out of bounds" (λ () (top-interp '{substring "" 0 10})))
(check-exn #rx"ZODE: Division by zero" (λ () (top-interp '{/ 1 0})))

; parse-type tests
(check-equal? (parse-type 'num) 'num)
(check-equal? (parse-type 'str) 'str)
(check-equal? (parse-type 'bool) 'bool)
(check-equal? (parse-type '{bool str num -> num}) (LamT '(bool str num) 'num))
(check-exn #rx"ZODE: Not a valid type" (λ () (parse-type 'hi)))

; type-check tests
(check-equal? (type-check (NumC 10) '()) 'num)
(check-equal? (type-check (IdC 'x) (list (tBind 'x 'num))) 'num)
(check-equal? (type-check (StrC "ten") '()) 'str)
(check-equal? (type-check (IdC 'true) base-tenv) 'bool)
(check-equal? (type-check (IfC (IdC 'false) (StrC "zero") (StrC "one")) base-tenv) 'str)
(check-exn #rx"ZODE: 'test' statement must be a bool in 'if'"
           (λ () (type-check (IfC (NumC 0) (StrC "zero") (StrC "one")) base-tenv)))
(check-exn #rx"ZODE: 'then' and 'else'" (λ () (type-check (IfC (IdC 'true) (StrC "zero") (NumC 5)) base-tenv)))
(check-equal? (type-check (LambC '(x) '(num) 'bool (IdC 'true)) base-tenv) (LamT '(num) 'bool))
(check-equal? (type-check (LambC '(x) '(num) #f (IdC 'true)) base-tenv) (LamT '(num) 'bool))
(check-exn #rx"ZODE: lambda type mismatch" (λ () (type-check (LambC '(x) '(num) 'str (NumC -9)) '())))
(check-equal? (type-check (AppC (LambC '(x) '(num) 'bool (IdC 'true)) (list (NumC 0))) base-tenv) 'bool)
(check-exn #rx"ZODE: Arg types do not match lambda expected types" (λ () (type-check (parse '{+ 4 "abc"}) base-tenv)))
(check-exn #rx"ZODE: Expected first argument of application"
           (λ () (type-check (AppC (NumC 0) (list (IdC 'x))) (list (tBind 'x 'num)))))
(check-equal? (type-check (parse '{locals-rec
                                   : square-helper = {lamb : [num n] -> num
                                                           : {if : {<= n 0}
                                                                 : 0
                                                                 : {+ n {square-helper {- n 2}}}}}
                                   : {locals : {num -> num} square = {lamb : [num n] -> num
                                                                           : {square-helper {- {* 2 n} 1}}}
                                             : {square 13}}})
                          base-tenv)
              'num)
(check-exn #rx"ZODE: lambda in locals-rec"
           (λ () (type-check (parse '{locals-rec
                                      : foo = {lamb : [num n] -> num
                                                    : "hi"}
                                      : 0})
                             base-tenv)))

; parse-rec tests
(check-equal? (parse '{locals-rec
                       : square-helper = {lamb : [num n] -> num
                                               : {if : {<= n 0}
                                                     : 0
                                                     : {+ n {square-helper {- n 2}}}}}
                       : {locals : {num -> num} square = {lamb : [num n] -> num
                                                               : {square-helper {- {* 2 n} 1}}}
                                 : {square 13}}})
              (RecC 'square-helper
                    (LambC '(n) '(num) 'num
                           (IfC (AppC (IdC '<=)
                                      (list (IdC 'n) (NumC 0)))
                                (NumC 0)
                                (AppC (IdC '+)
                                      (list (IdC 'n)
                                            (AppC (IdC 'square-helper)
                                                  (list (AppC (IdC '-)
                                                              (list (IdC 'n) (NumC 2)))))))))
                    (AppC (LambC '(square) (list (LamT '(num) 'num)) #f
                                 (AppC (IdC 'square) (list (NumC 13))))
                          (list (LambC '(n) '(num) 'num
                                       (AppC (IdC 'square-helper)
                                             (list (AppC (IdC ' -)
                                                         (list (AppC (IdC '*)
                                                                     (list (NumC 2) (IdC 'n)))
                                                               (NumC 1))))))))))
(check-exn #rx"ZODE: Invalid use of 'locals-rec'" (λ () (parse '{locals-rec 'no})))
