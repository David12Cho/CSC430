; Fully implemented, some unknown errors.

#lang typed/racket
(require typed/rackunit)

; define type for ExprC
(define-type ExprC (U NumC IdC StrC IfC LambC AppC))

; structs for ExprC types
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct StrC ([val : String]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LambC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)


; takes in an Sexp and converts it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? sym) (if (member sym '(if lamb locals : =)) 
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
                             (if (check-duplicates (first clauses))
                                 (error 'parse-locals "ZODE: Cannot have two parameters with the same name: ~e" clauses)
                                 (AppC (LambC (cast (first clauses)
                                                    (Listof Symbol))
                                              (parse (last r)))
                                       (cast (second clauses)
                                             (Listof ExprC)))))]))

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
    [(? real? n)  (if (> n 4)
                     (cons (take s 4) (split-clauses (drop s 4)))
                     (error 'split-clauses "ZODE: Incorrect number of parameters in clauses: ~e" s))]))


; takes in an Sexp and converts it into a LambC
(define (parse-lamb [s : Sexp]) : LambC
  (match s
    [(list ': (? symbol? params) ... ': body)
     (if (check-duplicates params)
         (error 'parse-lamb "ZODE: Same name is used for multiple parameters in: ~e" s)
         (LambC (cast params (Listof Symbol)) (parse body)))]
    [other (error 'parse-lamb "ZODE: Invalid use of 'lamb': lamb ~e" s)]))


; takes in an Sexp and converts it into an AppC
(define (parse-app [s : Sexp]) : AppC
  (match s
    [(list func args ...) (AppC (parse func)
                                (for/list  : (Listof ExprC)
                                  ([arg args])
                                  (parse (cast arg Sexp))))]))

; define types for environments
(struct Bind ([name : Symbol] [val : Integer]) #:transparent)
(define-type Env (Listof Bind))

; define store
(define-type Store (Mutable-Vectorof Value))

; define Value type
(define-type Value (U Real String Boolean Store CloV PrimOpV))
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimOpV ([proc : ((Listof ExprC) Env Store -> Value)] [args : Integer]) #:transparent)

; takes in a list of ExprC, evaluates them and returns the last value
(define (seq [args : (Listof ExprC)] [env : Env] [sto : Store]) : Value
  (match args
    [(cons f '()) (interp f env sto)]
    [(cons f r) (begin (interp f env sto)
                       (seq r env sto))]
    [other (error 'seq "ZODE: seq requires at least one expression to evaluate")]))


; defines top environment
(define top-env
  (list
   (Bind '+ 1)
   (Bind '- 2)
   (Bind '* 3)
   (Bind '/ 4)
   (Bind '<= 5)
   (Bind 'equal? 6)
   (Bind 'error 7)
   (Bind 'seq 8)
   (Bind 'make-array 9)
   (Bind 'array 10)
   (Bind 'aref 11)
   (Bind 'aset! 12)
   (Bind 'substring 13)
   (Bind 'true 14)
   (Bind 'false 15)))


; constructs top level store
(define (make-top-store [size : Natural]) : Store
  (let ([sto : Store (make-vector size)])
    (vector-set! sto (ann 1 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                          (let ([arg1 (interp (car args) env sto)] [arg2 (interp (cadr args) env sto)])
                            (if (and (real? arg1) (real? arg2))
                                (+ arg1 arg2)
                                (error "ZODE: Arguments to + must be numbers.")))) 
                   2))
    (vector-set! sto (ann 2 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                          (let ([arg1 (interp (car args) env sto)] [arg2 (interp (cadr args) env sto)])
                            (if (and (real? arg1) (real? arg2))
                                (- arg1 arg2)
                                (error "ZODE: Arguments to - must be numbers."))))
                   2))
    (vector-set! sto (ann 3 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                          (let ([arg1 (interp (car args) env sto)] [arg2 (interp (cadr args) env sto)])
                            (if (and (real? arg1) (real? arg2))
                                (* arg1 arg2)
                                (error "ZODE: Arguments to * must be numbers."))))
                   2))
    (vector-set! sto (ann 4 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                          (let ([arg1 (interp (car args) env sto)] [arg2 (interp (cadr args) env sto)])
                            (if (and (real? arg1) (real? arg2))
                                (if (= arg2 0)
                                    (error "ZODE: Division by zero.")
                                    (/ arg1 arg2))
                                (error "ZODE: Arguments to / must be numbers."))))
                   2))
    (vector-set! sto (ann 5 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                           (let ([arg1 (interp (car args) env sto)] [arg2 (interp (cadr args) env sto)])
                             (if (and (real? arg1) (real? arg2))
                                 (<= arg1 arg2)
                                 (error "ZODE: Arguments to <= must be numbers."))))
                      2))
    (vector-set! sto (ann 6 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                (let ([arg1 (interp (car args) env sto)] [arg2 (interp (cadr args) env sto)])
                                  (equal? arg1 arg2)))
                          2))
    (vector-set! sto (ann 7 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                               (let ([arg1 (interp (car args) env sto)])
                                 (error "ZODE: user-error: ~e" (serialize arg1))))
                         1))
    (vector-set! sto (ann 8 Natural) (PrimOpV (λ ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                                (seq args env sto))
                                              -1))
    (vector-set! sto (ann 9 Natural) (PrimOpV (λ ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                                (let ([arg1 (interp (car args) env sto)]
                                                      [arg2 (interp (cadr args) env sto)])
                                                  (if (not (exact-integer? arg1))
                                                      (error 'make-array "ZODE: Expected whole number as first argument: ~e" arg1)
                                                      (if (< arg1 1)
                                                          (error 'make-array "ZODE: Cannot make array smaller than size 1")
                                                          (let ([free (vector-ref sto (ann 0 Natural))])
                                                            (if (exact-integer? free)
                                                                (let ([new-arr (make-vector arg1 arg2)])
                                                                  (vector-set! sto free new-arr)
                                                                  new-arr)
                                                                (error 'make-array "ZODE: store free index not valid: ~e" free)))))))
                                              2))
    (vector-set! sto (ann 10 Natural) (PrimOpV (λ ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                                 (if (< (length args) 1)
                                                     (error 'array "ZODE: Cannot make array smaller than size 1")
                                                     (let ([free (vector-ref sto (ann 0 Natural))])
                                                       (if (exact-integer? free)
                                                           (let ([new-arr (list->vector (for/list : (Listof Value) ([arg args])
                                                                                          (interp arg env sto)))])
                                                             (vector-set! sto free new-arr)
                                                             new-arr)
                                                           (error 'array "ZODE: store free index not valid: ~e" free)))))
                                               -1))
    (vector-set! sto (ann 11 Natural) (PrimOpV (λ ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                                 (let ([arg1 (interp (car args) env sto)]
                                                       [arg2 (interp (cadr args) env sto)])
                                                   (if (not (vector? arg1))
                                                       (error 'aref "ZODE: Expected array as first argument: ~e" arg1)
                                                       (if (not (exact-integer? arg2))
                                                           (error 'aref "ZODE: Expected whole number as second argument: ~e" arg2)
                                                           (if (or (< arg2 0) (>= arg2 (vector-length arg1)))
                                                               (error 'aref "ZODE: Array index out of bounds: ~e" arg2)
                                                               (vector-ref arg1 (ann arg2 Natural)))))))
                                               2))
    (vector-set! sto (ann 12 Natural) (PrimOpV (λ ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                                 (let ([arg1 (interp (car args) env sto)]
                                                       [arg2 (interp (cadr args) env sto)]
                                                       [arg3 (interp (caddr args) env sto)])
                                                   (if (not (vector? arg1))
                                                       (error 'aset! "ZODE: Expected array as first argument: ~e" arg1)
                                                       (if (not (exact-integer? arg2))
                                                           (error 'aset! "ZODE: Expected whole number as second argument: ~e" arg2)
                                                           (if (or (< arg2 0) (>= arg2 (vector-length arg1)))
                                                               (error 'aset! "ZODE: Array index out of bounds: ~e" arg2)
                                                               (vector-set! arg1 (ann arg2 Natural) arg3))))
                                                   #t))
                                               3))
    (vector-set! sto (ann 13 Natural) (PrimOpV (lambda ([args : (Listof ExprC)] [env : Env] [sto : Store])
                                                 (let ([arg1 (interp (car args) env sto)]
                                                       [arg2 (interp (cadr args) env sto)]
                                                       [arg3 (interp (caddr args) env sto)])
                                                   (if (not (string? arg1))
                                                       (error 'substring "ZODE: Expected string as first argument: ~e" arg1)
                                                       (if (not (exact-integer? arg2))
                                                           (error 'substring "ZODE: Expected whole number as second argument: ~e" arg2)
                                                       (if (not (exact-integer? arg3))
                                                           (error 'substring "ZODE: Expected whole number as third argument: ~e" arg3)
                                                           (if (or (> arg2 arg3)
                                                                   (< arg2 0)
                                                                   (>= arg2 (string-length arg1))
                                                                   (>= arg3 (string-length arg1)))
                                                               (error 'substring "ZODE: Array index out of bounds: ~e to ~e" arg2 arg3)
                                                               (substring arg1 arg2 arg3)))))))
                                               3))
    (vector-set! sto (ann 14 Natural) #t)
    (vector-set! sto (ann 15 Natural) #f)
    (vector-set! sto (ann 0 Natural) 16)
    sto))


; takes in a list of parameters and a list of arguments and extends an environment by binding the corresponding values
(define (extend-env [params : (Listof Symbol)] [args : (Listof ExprC)] [env : Env] [sto : Store]) : (Pairof Env Store)
  (match* (params args)
    [('() '()) (cons env sto)]
    [((cons first-param rest-param) (cons first-arg rest-arg)) (let ([free (vector-ref sto (ann 0 Natural))])
                                                                 (if (exact-integer? free)
                                                                     (begin
                                                                       (vector-set! sto free (interp first-arg env sto))
                                                                       (vector-set! sto (ann 0 Natural)
                                                                              (+ free 1)))
                                                                     (error 'extend-env "ZODE: free index not valid: ~e" free))                                                          
                                                                 (let ([rec (extend-env rest-param
                                                                                        rest-arg env sto)])
                                                                   (cons (cons (Bind first-param free)
                                                                               (car rec))
                                                                         (cdr rec))))]
    [(some other) (error 'extend-env "ZODE: Incorrect number of arguments, expected ~e, got ~e" params args)]))


; takes in a Symbol and returns the corresponding value in the environment if it exists
(define (lookup [for : Symbol] [env : Env] [sto : Store]) : Value
  (match env
    ['() (error 'lookup "ZODE: Variable name not found: ~e" for)]
    [(cons f r) (if (symbol=? for (Bind-name f))
                    (vector-ref sto (Bind-val f))
                    (lookup for r sto))]))


; Adjust the interp function to handle expressions directly without pre-evaluating arguments for closures
(define (interp [e : ExprC] [env : Env] [sto : Store]) : Value
  (match e
    [(NumC n) n]
    [(IdC sym) (lookup sym env sto)]
    [(StrC str) str]
    [(IfC test then else) 
     (let ([test-result : Value (interp test env sto)])
       (if (boolean? test-result)
           (if test-result
               (interp then env sto)
               (interp else env sto))
           (error 'interp "ZODE: if test is not a boolean: ~e" e)))]
    [(LambC params body) (CloV params body env)]
    [(AppC func args) 
     (let ([f : Value (interp func env sto)])
       (match f
         [(CloV params body closure-env)
          (cond
            [(= (length params) (length args))
             (let ([ext (extend-env params args closure-env sto)])
                        (interp body (car ext) (cdr ext)))]
            [else (error 'interp "ZODE: Incorrect number of arguments for function: expected ~a, got ~a" (length params)
                         (length args))])]
         [(PrimOpV proc arity)
          (if (or (equal? -1 arity)
                  (equal? arity (length args)))
              (proc args env sto)
              (error 'interp "ZODE: Incorrect number of arguments for primop: expected ~a, got ~a" arity
                     (length args)))]
         [else
          (error 'interp "ZODE: Cannot apply as a function: ~e" f)]))]))



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
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  (serialize (interp (parse s) top-env (make-top-store memsize))))




; parse tests
(check-equal? (parse -2.2) (NumC -2.2))
(check-equal? (parse 'hi) (IdC 'hi))
(check-equal? (parse "string") (StrC "string"))
(check-exn #rx"ZODE: Invalid use of reserved keyword as an identifier" (λ () (parse 'if)))
(check-exn #rx"ZODE: Invalid concrete syntax, cannot parse" (λ () (parse #f)))
(check-exn #rx"ZODE: Cannot have two parameters with the same name"
           (λ () (parse '{locals : z = {lamb : : 3} : z = 9 : {z}})))

; parse-if tests
(check-equal? (parse '{if : 0 : b : "hi"}) (IfC (NumC 0) (IdC 'b) (StrC "hi")))
(check-exn #rx"ZODE: Invalid use of 'if'" (λ () (parse  '{if : not : enough})))

; parse-locals tests
(check-equal? (parse '{locals : var = "var"
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
(check-equal? (parse '{lamb : : 0}) (LambC '() (NumC 0)))
(check-equal? (parse '{lamb : a b c : "hello"}) (LambC '(a b c) (StrC "hello")))
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
                           (IdC 'false))
                      top-env
                      (make-top-store 100))
              0)
(check-exn #rx"ZODE: Cannot apply as a function" (λ () (interp (IfC (IdC 'false)
                                                                    (IdC 'bye)
                                                                   (AppC (NumC pi) '()))
                                                               top-env
                                                               (make-top-store 20))))
                                        
; lookup tests
(check-equal? (lookup 'here (list (Bind 'here 0) (Bind 'nothere 1)) (vector #t "nope")) #t)
(check-exn #rx"ZODE: Variable name not found" (λ () (lookup 'dne (list (Bind 'lock 10)) (vector))))

; extend-env tests
(check-equal? (car (extend-env '(a b c)
                                (list (NumC 1) (NumC 2) (NumC 3))
                                top-env
                                (make-top-store 20)))
              (append (list (Bind 'a 16) (Bind 'b 17) (Bind 'c 18))
                      top-env))
(check-exn #rx"ZODE: Incorrect number of arguments" (λ () (extend-env '(x) '() '() (vector 0))))
(check-exn #rx"ZODE: free index not valid" (λ () (extend-env '(x) (list (NumC 0)) top-env (vector "hi"))))

; serialize test
#;(check-equal? (serialize (PrimOpV (λ (args) ) 3) "#<primop>"))

; top-interp tests
(check-equal? (top-interp '{locals : x = 5
                              : {+ x 2}} 100) "7")
(check-equal? (top-interp '{locals : x = 5
                                   : "hi"} 100) "\"hi\"")
(check-equal? (top-interp '{locals : x = 5
                                   : true} 100) "true")
(check-equal? (top-interp '{locals : x = 5
                                   : false} 100) "false")
(check-equal? (top-interp '{locals : x = 5
                                   : (lamb : : 0)} 100) "#<procedure>")
(check-equal? (top-interp '{locals : x = 0
                                   : {if : {<= {+ 1 2} {- 4 3}}
                                         : "hi"
                                         : {if : {equal? 1 1}
                                               : {* {/ 2 2} 1}
                                               : false}}} 100) "1")
(check-exn #rx"ZODE: if test is not a boolean" (λ () (top-interp '{if : {+ 3 4} : 8 : 7} 100)))

; primop tests
(check-exn #rx"ZODE: Arguments to +" (λ () (top-interp '{+ "true" "false"} 100)))
(check-exn #rx"ZODE: Arguments to -" (λ () (top-interp '{- "true" "false"} 100)))
(check-exn #rx"ZODE: Arguments to *" (λ () (top-interp '{* "true" "false"} 100)))
(check-exn #rx"ZODE: Arguments to /" (λ () (top-interp '{/ "true" "false"} 100)))
(check-exn #rx"ZODE: Arguments to <=" (λ () (top-interp '{<= "true" "false"} 100)))
(check-exn #rx"ZODE: Division by zero" (λ () (top-interp '{/ 1 0} 100)))
(check-exn #rx"ZODE: user-error" (λ () (top-interp '{error "error"} 100)))
(check-exn #rx"ZODE: Incorrect number of arguments for function" (λ () (top-interp '{{lamb : : 0} 1} 100)))
(check-exn #rx"ZODE: Incorrect number of arguments for primop" (λ () (top-interp '{+ 1} 100)))
;(check-equal? (top-interp '{read-num}) "100")
;(check-equal? (top-interp '{read-str}) "\"hundred\"")
;(check-exn #rx"ZODE: Input is not a real" (λ () (top-interp '{read-num})))
(check-equal? (top-interp '{seq 10 20} 100) "20")
(check-exn #rx"ZODE: seq requires at least" (λ () (top-interp '{seq} 100)))

; array tests
(check-equal? (top-interp '{seq {make-array 5 0.0} "hi"} 17) "\"hi\"")
(check-exn #rx"ZODE: Cannot make array smaller" (λ () (top-interp '{make-array 0 "no"} 50)))
(check-exn #rx"ZODE: Expected whole number" (λ () (top-interp '{make-array "fail" 888} 50)))
(check-equal? (top-interp '{seq {array "nah" 10} 8} 50) "8")
(check-exn #rx"ZODE: Cannot make array smaller" (λ () (top-interp '{array} 50)))
(check-equal? (top-interp '{locals : arr = {make-array 3 "test"}
                                   : {aref arr 2}} 50) "\"test\"")
(check-exn #rx"ZODE: Expected array" (λ () (top-interp '{aref "him" 1} 50)))
(check-exn #rx"ZODE: Expected whole number" (λ () (top-interp '{locals : arr = {make-array 1 1}
                                                                       : {aref arr "nope"}} 50)))
(check-exn #rx"ZODE: Array index out of bounds" (λ () (top-interp '{locals : arr = {make-array 1 1}
                                                                       : {aref arr 10}} 50)))
(check-equal? (top-interp '{locals : arr = {make-array 3 "test"}
                                   : {seq {aset! arr 2 -1}
                                          {aref arr 2}}} 50) "-1")
(check-exn #rx"ZODE: Expected array" (λ () (top-interp '{aset! "him" 1 "pi"} 50)))
(check-exn #rx"ZODE: Expected whole number" (λ () (top-interp '{locals : arr = {make-array 1 1}
                                                                       : {aset! arr "nope" "val"}} 50)))
(check-exn #rx"ZODE: Array index out of bounds" (λ () (top-interp '{locals : arr = {make-array 1 1}
                                                                       : {aset! arr 10 "val"}} 50)))
(let ([test-sto (make-top-store 20)])
  (vector-set! test-sto (ann 0 Natural) "oops")
  (check-exn #rx"ZODE: store free index not valid" (λ () (interp (parse '{make-array 1 1}) top-env test-sto)))
  (check-exn #rx"ZODE: store free index not valid" (λ () (interp (parse '{array 1 1}) top-env test-sto))))
(check-equal? (top-interp '{locals : str = "abcdefg"
                                   : {substring str 2 5}} 50) "\"cde\"")
(check-exn #rx"ZODE: Expected string" (λ () (top-interp '{substring 0 1 "pi"} 50)))
(check-exn #rx"ZODE: Expected whole number as second" (λ () (top-interp '{locals : str = "abcdefg"
                                                                       : {substring str "yee" "nah"}} 50)))
(check-exn #rx"ZODE: Expected whole number as third" (λ () (top-interp '{locals : str = "abcdefg"
                                                                       : {substring str 0 "nah"}} 50)))
(check-exn #rx"ZODE: Array index out of bounds" (λ () (top-interp '{locals : str = "abcdefg"
                                                                       : {substring str 10 9}} 50)))
