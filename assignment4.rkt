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
    [(cons ': (? list? r)) (let ([clauses (parse-clauses (split-clauses (cast (drop-right r 1) (Listof Sexp))))])
                   (AppC (LambC (cast (first clauses) (Listof Symbol)) (parse (last r))) (cast (second clauses) (Listof ExprC))))]))

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
    [(list ': (? symbol? params) ... ': body) (if (check-duplicates params)
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
(check-equal? (parse '{locals : var = "var" : pi = 3.14 : pi}) (AppC (LambC '(var pi) (IdC 'pi)) (list (StrC "var") (NumC 3.14))))
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