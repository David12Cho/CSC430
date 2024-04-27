#lang typed/racket
(require typed/rackunit)

;;define type for ExprC
(define-type ExprC (U NumC BinopC IdC AppC ifleq0?))

;;structs for each type of ExprC possible
(struct NumC ([n : Real]) #:transparent)
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct AppC ([func : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)


;;takes in a s-expression and converts it into a ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [{? real? num} (NumC num)]
    [(list op l r) 
     (if (member op '(+ - * /)) 
         (BinopC op (parse l) (parse r))  ; Parse l and r here, not in the pattern
         (error "ZODE: Invalid operator: ~a" op))]
    [{? symbol? sym}
     (if (member sym '(def ifleq0? :)) 
         (error "ZODE: Invalid use of reserved keyword as an identifier: ~a" sym)
         (IdC sym))]
    [`(ifleq0? ,test ,then ,else)
     (ifleq0? (parse test) (parse then) (parse else))]
    [(list funcName args ...)
     (if (symbol? funcName)
         (AppC funcName (for/list : (Listof ExprC) ([arg (in-list args)])
                                   (parse arg)))
         (error "Expected a function name, got a non-symbol: ~a" funcName))]

    [else (error "ZODE: Expression is unrecognized: ~a" s)]
    ))  


 
;;parse tests
(check-equal? (parse 2) (NumC 2))
(check-equal? (parse 'a) (IdC 'a))
(check-equal? (parse '(+ 2 3)) (BinopC '+ (NumC 2) (NumC 3)))
(check-equal? (parse '(- 3 1)) (BinopC '- (NumC 3) (NumC 1))) 
(check-equal? (parse '(* 7 4)) (BinopC '* (NumC 7) (NumC 4))) 
(check-equal? (parse '(/ 5 6)) (BinopC '/ (NumC 5) (NumC 6)))
(check-equal? (parse '(sum 1 2 3)) (AppC 'sum (list (NumC 1) (NumC 2) (NumC 3))))
(check-equal? (parse '(* (+ 1 2) (- 3 4)))(BinopC '*
                                                   
                      (BinopC '+ (NumC 1) (NumC 2))
                      (BinopC '- (NumC 3) (NumC 4))))
(check-equal? (parse '{main 8}) (AppC 'main (list (NumC 8))))

(check-equal? (parse '{ifleq0? x x {- x 1}})
              (ifleq0? (IdC 'x) (IdC 'x) (BinopC '- (IdC 'x) (NumC 1))))

(check-equal? (parse '{ifleq0? 2 1 {- 10 1}})
              (ifleq0? (NumC 2) (NumC 1) (BinopC '- (NumC 10) (NumC 1))))

(check-exn #rx"ZODE" (lambda () (parse '(1 2 3))))
(check-equal? (parse 'abcdefg)(IdC 'abcdefg))
(check-exn #rx"ZODE" (lambda () (parse '(123 1 2))))
(check-exn #rx"ZODE"(lambda () (parse '())))
(check-exn #rx"ZODE" (lambda () (parse 'def)))
(check-exn #rx"ZODE" (lambda () (parse 'ifleq0?)))








