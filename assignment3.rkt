#lang typed/racket
(require typed/rackunit)

;;define type for ExprC
(define-type ExprC (U NumC BinopC IdC AppC ifleq0?))

;;structs for each type of ExprC possible
(struct NumC ([n : Real]) #:transparent)
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
;;(struct AppC ([func : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct AppC ([func : Symbol] [arg : ExprC]) #:transparent)
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

;;FundefC of one argument
(struct FundefC ([name : Symbol] [param : Symbol] [body : ExprC]) #:transparent)


;;takes in a s-expression and converts it into a ExprC
#;(define (parse [s : Sexp]) : ExprC
  (match s
    [{? real? num} (NumC num)]
    [(list (? symbol? op) l r) 
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
         (error "ZODE: Expected a function name, got a non-symbol: ~a" funcName))]

    [else (error "ZODE: Expression is unrecognized: ~a" s)]
    ))
(define (parse [s : Sexp]) : ExprC
  (match s
    [{? real? num} (NumC num)]
    [(list (? symbol? op) l r) 
     (if (member op '(+ - * /)) 
         (BinopC op (parse l) (parse r))  ; Parse l and r here, not in the pattern
         (error "ZODE: Invalid operator: ~a" op))]
    [{? symbol? sym}
     (if (member sym '(def ifleq0? :)) 
         (error "ZODE: Invalid use of reserved keyword as an identifier: ~a" sym)
         (IdC sym))]
    [`(ifleq0? ,test ,then ,else)
     (ifleq0? (parse test) (parse then) (parse else))]
    [(list funcName arg)
     (if (symbol? funcName)
         (AppC funcName (parse arg))
         (error "ZODE: Expected a function name, got a non-symbol: ~a" funcName))]

    [else (error "ZODE: Expression is unrecognized: ~a" s)]
    ))

;;takes in an ExprC and interprets it to a value
(define (interp [e : ExprC] [fds : (Listof FundefC)]) : Real
  (match e
    [(NumC n) n]
    [(BinopC op l r) (compute op (interp l fds) (interp r fds))]
    [(IdC name) (error 'interp "Interp shouldn't get here, given IdC: ~a" e)]
    [(AppC f arg) (interp (subst (interp arg fds)
                                 (FundefC-param (get-fundef f fds))
                                 (FundefC-body (get-fundef f fds)))
                          fds)]
    [(ifleq0? test then else) (if (<= (interp test fds) 0)
                                  (interp then fds)
                                  (interp else fds))]
    [other (error 'interp "ZODE: ExprC is invalid: ~a" e)]))


;;takes in a symbol and list of functions and returns a FundefC if the symbol is a name of one of the functions
(define (get-fundef [sym : Symbol] [funcs : (Listof FundefC)]) : FundefC
  (match funcs
    [(cons (struct* FundefC ([name n] [param _] [body _])) r) (if (equal? sym n)
                                                                (first funcs)
                                                                (get-fundef sym r))]
    [else (error 'get-fundef "ZODE: Function name not recognized: ~a" sym)]))


;takes in a Real to substituted in, a Symbol to be replaced, and an Expr to modify, and returns the altered ExprC
(define (subst [what : Real] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC _) in]
    [(BinopC op l r) (BinopC op (subst what for l)
                        (subst what for r))]
    [(IdC s) (if (symbol=? s for)
                 (NumC what)
                 in)]
    [(AppC f a) (AppC f (subst what for a))]
    [(ifleq0? test then else) (ifleq0? (subst what for test)
                                       (subst what for then)
                                       (subst what for else))]))


;;takes in a binary operator and two operands and returns the result of applying the right on the left
(define (compute [op : Symbol] [l : Real] [r : Real]) : Real
  (match op
    ['+ (+ l r)]
    ['- (- l r)]
    ['* (* l r)]
    ['/ (/ l r)]
    [other (error 'compute "ZODE: Invalid operator: ~a" op)]))


;;combines parsing and evaluation
#;(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;get-fundef tests
(define f (FundefC 'five 'x (NumC 5)))
(define g (FundefC 'hi 'y (IdC 'hello)))
(check-equal? (get-fundef 'five (list g f)) f)
(check-exn #rx"ZODE: Function name not recognized" (λ () (get-fundef 'dne (list g))))


;interp tests
(check-equal? (interp (ifleq0? (AppC 'five
                                     (NumC 0))
                               (NumC 2)
                               (BinopC '+
                                       (NumC 1)
                                       (NumC 2)))
                      (list f g)) 2)

;subst tests
(check-equal? (subst 0
                     'zero
                     (ifleq0? (AppC 'f
                                    (IdC 'zero))
                              (BinopC '+
                                      (NumC 1)
                                      (IdC 'hi))
                              (NumC 2)))
              (ifleq0? (AppC 'f
                             (NumC 0))
                       (BinopC '+
                               (NumC 1)
                               (IdC 'hi))
                       (NumC 2)))


;;(compute) tests
(check-= (compute '+ -3 0.5) -2.5 0.1)
(check-= (compute '- pi -6) 9.14 0.01)
(check-= (compute '* 20 -0.2) -4 1)
(check-= (compute '/ -12 4) -3 3)
(check-exn #rx"ZODE: Invalid operator" (λ () (compute 'hi 0 0)))

 
;;parse tests
(check-equal? (parse 2) (NumC 2))
(check-equal? (parse 'a) (IdC 'a))
(check-equal? (parse '(+ 2 3)) (BinopC '+ (NumC 2) (NumC 3)))
(check-equal? (parse '(- 3 1)) (BinopC '- (NumC 3) (NumC 1))) 
(check-equal? (parse '(* 7 4)) (BinopC '* (NumC 7) (NumC 4))) 
(check-equal? (parse '(/ 5 6)) (BinopC '/ (NumC 5) (NumC 6)))
;(check-equal? (parse '(sum 1 2 3)) (AppC 'sum (list (NumC 1) (NumC 2) (NumC 3))))
(check-equal? (parse '(sum 1 )) (AppC 'sum (NumC 1)))
(check-equal? (parse '(* (+ 1 2) (- 3 4)))(BinopC '*
                                                   
                      (BinopC '+ (NumC 1) (NumC 2))
                      (BinopC '- (NumC 3) (NumC 4))))
;(check-equal? (parse '{main 8}) (AppC 'main (list (NumC 8))))
(check-equal? (parse '{main 8}) (AppC 'main (NumC 8)))

(check-equal? (parse '{ifleq0? x x {- x 1}})
              (ifleq0? (IdC 'x) (IdC 'x) (BinopC '- (IdC 'x) (NumC 1))))

(check-equal? (parse '{ifleq0? 2 1 {- 10 1}})
              (ifleq0? (NumC 2) (NumC 1) (BinopC '- (NumC 10) (NumC 1))))

(check-exn #rx"ZODE: Invalid operator" (lambda () (parse '(a 2 3))))
(check-equal? (parse 'abcdefg)(IdC 'abcdefg))
(check-exn #rx"ZODE: Expected a function name" (lambda () (parse '(123 1))))
(check-exn #rx"ZODE: Expression is unrecognized"(lambda () (parse '())))
(check-exn #rx"ZODE: Invalid use of reserved keyword as an identifier" (lambda () (parse 'def)))
(check-exn #rx"ZODE: Invalid use of reserved keyword as an identifier" (lambda () (parse 'ifleq0?)))
(check-exn #rx"ZODE: Expected a function name" (λ () (parse '(1 'hi))))








