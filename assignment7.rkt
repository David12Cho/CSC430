#lang typed/racket
(require typed/rackunit)

; define type for ExprC
(define-type ExprC (U NumC IdC StrC IfC LambC AppC))

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
                   (AppC (LambC (cast (first clauses)
                                      (Listof Symbol))
                                (parse (last r)))
                         (cast (second clauses)
                               (Listof ExprC))))]))

; takes in an Sexp and converts it into a list of parameters (Symbols) and a list of their values (ExprC)
(define (parse-clauses [s : (Listof Sexp)]) :  (Listof (U (Listof Symbol) (Listof ExprC)))
  (match s
    [(list (list (? symbol? ids) '= vals ':) ...) (list (cast ids (Listof Symbol)) (for/list : (Listof ExprC)
                                                                          ([val vals])
                                                                          (parse (cast val Sexp))))]
    [other (error 'parse-clauses "ZODE: Clauses not formatted correctly: ~e" s)]))

; takes in a list of clauses and splits them into their own lists
(define (split-clauses [s : (Listof Sexp)]) : (Listof (Listof Sexp))
  (match (length s)
    [4 (list s)]
    [(? real? n) (if (> n 4)
                     (cons (take s 4) (split-clauses (drop s 4)))
                     (error 'split-clauses "ZODE: Incorrect number of parameters in clauses: ~e" s))]))


; takes in an Sexp and converts it into a LambC
(define (parse-lamb [s : Sexp]) : LambC
  (match s
    [(list ': (? symbol? params) ... types ... ret ': body)
     (if (check-duplicates params)
         (error 'parse-lamb "ZODE: Same name is used for multiple parameters in: ~e" s)
         (LambC (cast params (Listof Symbol))
                (for/list : (Listof Type)
                  ([type types])
                  (parse-type type))
                (parse-type ret)
                (parse body)))]
    [other (error 'parse-lamb "ZODE: Invalid use of 'lamb': lamb ~e" s)]))


; takes in an Sexp and converts it into an AppC
(define (parse-app [s : Sexp]) : AppC
  (match s
    [(list func args ...) (AppC (parse func)
                                (for/list  : (Listof ExprC)
                                  ([arg args])
                                  (parse (cast arg Sexp))))]))


; data definition for types
(define-type Type (U "num" "str" "bool" LamT))
(struct LamT ([in : (Listof Type)] [out : Type]) #:transparent)

(define (parse-type [s : Sexp]) : Type
  (match s
    ["num" "num"]
    ["str" "str"]
    ["bool" "bool"]
    [(list args ... '-> ret) (LamT (for/list : (Listof Type)
                                               ([arg args])
                                               (parse-type (cast arg Sexp)))
                                   (parse-type ret))]
    [other (error 'parse-type "ZODE: Not a valid type: ~e" s)]))


; define types for environments
(struct Bind ([name : Symbol] [val : (U Type Value)]) #:transparent)
(define-type Env (Listof Bind))

; define Value type
(define-type Value (U Real String Boolean CloV PrimOpV))
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimOpV ([proc : ((Listof Value) -> Value)] [args : Integer]) #:transparent)

(define top-env
  (list
   (Bind '+ (PrimOpV (lambda (args)
                          (let ([arg1 (car args)] [arg2 (cadr args)])
                            (if (and (real? arg1) (real? arg2))
                                (+ arg1 arg2)
                                (error "ZODE: Arguments to + must be numbers.")))) 
                   2))
   (Bind '- (PrimOpV (lambda (args)
                          (let ([arg1 (car args)] [arg2 (cadr args)])
                            (if (and (real? arg1) (real? arg2))
                                (- arg1 arg2)
                                (error "ZODE: Arguments to - must be numbers."))))
                   2))
   (Bind '* (PrimOpV (lambda (args)
                          (let ([arg1 (car args)] [arg2 (cadr args)])
                            (if (and (real? arg1) (real? arg2))
                                (* arg1 arg2)
                                (error "ZODE: Arguments to * must be numbers."))))
                   2))
   (Bind '/ (PrimOpV (lambda (args)
                          (let ([arg1 (car args)] [arg2 (cadr args)])
                            (if (and (real? arg1) (real? arg2))
                                (if (= arg2 0)
                                    (error "ZODE: Division by zero.")
                                    (/ arg1 arg2))
                                (error "ZODE: Arguments to / must be numbers."))))
                   2))
   (Bind '<= (PrimOpV (lambda (args)
                           (let ([arg1 (car args)] [arg2 (cadr args)])
                             (if (and (real? arg1) (real? arg2))
                                 (<= arg1 arg2)
                                 (error "ZODE: Arguments to <= must be numbers."))))
                      2))
   (Bind 'equal? (PrimOpV (lambda (args)
                                (let ([arg1 (car args)] [arg2 (cadr args)])
                                  (equal? arg1 arg2)))
                          2))
   (Bind 'error (PrimOpV (lambda (args)
                               (let ([arg1 (car args)])
                                 (error "ZODE: user-error: ~e" (serialize arg1))))
                         1))
   (Bind 'true true)
   (Bind 'false false)
   ))


; takes in a list of parameters and a list of arguments and extends an environment by binding the corresponding values
(define (extend-env [params : (Listof Symbol)] [args : (Listof ExprC)] [env : Env]) : Env
  (match* (params args)
    [('() '()) env]
    [((cons first-param rest-param) (cons first-arg rest-arg)) (cons (Bind first-param
                                                                           (interp first-arg env))
                                                                     (extend-env rest-param
                                                                                 rest-arg env))]
    [(some other) (error 'extend-env "ZODE: Incorrect number of arguments, expected ~e, got ~e" params args)]))


; takes in a Symbol and returns the corresponding value in the environment if it exists
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "ZODE: Variable name not found: ~e" for)]
    [(cons f r) (if (symbol=? for (Bind-name f))
             (Bind-val f)
             (lookup for r))]))


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
          (cond
            [(= (length params) (length args))
              ; Pass expressions directly to extend-env and evaluate within it or in the body context
              (interp body (extend-env params args closure-env))]
            [else (error 'interp "ZODE: Incorrect number of arguments for function: expected ~a, got ~a" (length params)
                         (length args))])]
         [(PrimOpV proc arity)
          (if (= arity (length args))
              ; Evaluate args here because primops expect evaluated arguments
              (let ([evaluated-args : (Listof Value) (evaluate-arguments args env)])
                (proc evaluated-args))
              (error 'interp "ZODE: Incorrect number of arguments for primop: expected ~a, got ~a" arity
                     (length args)))]
         [else
          (error 'interp "ZODE: Cannot apply as a function: ~e" f)]))]))

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


; combines parse and interp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))




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
#;(check-equal? (parse '{locals : var = "var"
                              : pi = 3.14
                              : pi}) (AppC (LambC '(var pi)
                                                  (IdC 'pi))
                                           (list (StrC "var")
                                                 (NumC 3.14))))
(check-exn #rx"ZODE: Incorrect number of parameters in clauses" (λ () (parse '{locals : "hi" : 'm})))
(check-exn #rx"ZODE: Clauses not formatted correctly" (λ () (parse '{locals : 1 2 3 : 'no})))

; parse-clauses tests
(check-equal? (parse-clauses '{{x = 5 :} {y = "z" :}}) (list '(x y) (list (NumC 5) (StrC "z"))))
(check-equal? (parse-clauses '{{x = 5 :}}) (list '(x) (list (NumC 5))))

; split-clauses tests
(check-equal? (split-clauses '{x = 0 : y = 1 : z = 2 :}) '((x = 0 :) (y = 1 :) (z = 2 :)))

; parse-lamb tests
#;(check-equal? (parse '{lamb : : 0}) (LambC '() (NumC 0)))
#;(check-equal? (parse '{lamb : a b c : "hello"}) (LambC '(a b c) (StrC "hello")))
(check-exn #rx"ZODE: Invalid use of 'lamb'" (λ () (parse '{lamb fail})))
(check-exn #rx"ZODE: Same name is used for multiple parameters" (λ () (parse '{lamb : same same : u})))

; parse-app tests
(check-equal? (parse '{{lamb : y : 1} 0}) (AppC (LambC '(y) (NumC 1)) (list (NumC 0))))
(check-equal? (parse '{{lamb : z : test}}) (AppC (LambC '(z) (IdC 'test)) '()))

; interp tests
(check-equal? (interp (IfC (IdC 'true)
                           (AppC (LambC '(x y)
                                        (IdC 'x))
                                 (list (NumC 0)
                                       (StrC "dummy")))
                           (IdC 'false)) (list (Bind 'true #t)
                                               (Bind 'false #f)))
              0)
(check-exn #rx"ZODE: Cannot apply as a function" (λ () (interp (IfC (IdC 'false)
                                                                    (IdC 'bye)
                                                                   (AppC (NumC pi) '()))
                                                               (list (Bind 'false #f)))))
                                        
; lookup tests
(check-equal? (lookup 'here (list (Bind 'here #t) (Bind 'nothere "nope"))) #t)
(check-exn #rx"ZODE: Variable name not found" (λ () (lookup 'dne (list (Bind 'lock 10)))))

; extend-env tests
(check-equal? (extend-env '(a b c) (list (NumC 1) (NumC 2) (NumC 3)) '()) (list (Bind 'a 1) (Bind 'b 2) (Bind 'c 3)))
(check-exn #rx"Incorrect number of arguments" (λ () (extend-env '(x) '() '())))

; serialize test
#;(check-equal? (serialize (PrimOpV (λ (args) ) 3) "#<primop>"))

; top-interp tests
(check-equal? (top-interp '{locals : x = 5
                              : {+ x 2}}) "7")
(check-equal? (top-interp '{locals : x = 5
                                   : "hi"}) "\"hi\"")
(check-equal? (top-interp '{locals : x = 5
                                   : true}) "true")
(check-equal? (top-interp '{locals : x = 5
                                   : false}) "false")
(check-equal? (top-interp '{locals : x = 5
                                   : (lamb : : 0)}) "#<procedure>")
(check-equal? (top-interp '{locals : x = 0
                                   : {if : {<= {+ 1 2} {- 4 3}}
                                         : "hi"
                                         : {if : {equal? 1 1}
                                               : {* {/ 2 2} 1}
                                               : false}}}) "1")
(check-exn #rx"ZODE: Arguments to +" (λ () (top-interp '{+ "true" "false"})))
(check-exn #rx"ZODE: Arguments to -" (λ () (top-interp '{- "true" "false"})))
(check-exn #rx"ZODE: Arguments to *" (λ () (top-interp '{* "true" "false"})))
(check-exn #rx"ZODE: Arguments to /" (λ () (top-interp '{/ "true" "false"})))
(check-exn #rx"ZODE: Arguments to <=" (λ () (top-interp '{<= "true" "false"})))
(check-exn #rx"ZODE: Division by zero" (λ () (top-interp '{/ 1 0})))
(check-exn #rx"ZODE: user-error" (λ () (top-interp '{error "error"})))
(check-exn #rx"ZODE: Incorrect number of arguments for function" (λ () (top-interp '{{lamb : : 0} 1})))
(check-exn #rx"ZODE: Incorrect number of arguments for primop" (λ () (top-interp '{+ 1})))
