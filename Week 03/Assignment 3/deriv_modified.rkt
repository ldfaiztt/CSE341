#lang racket

;;; Chun-Wei Chen
;;; CSE 341
;;; Assignment 3 - Racket Warmup
;;; 10/17/12

;; The top-level function deriv takes an expression and a variable, 
;; and returns the derivative of that expression with respect to the variable,
;; in simplified form.
(define (deriv exp var)
  (simplify (basic-deriv exp var)))

;; Basic-deriv takes the derivative of an expression exp with respect to a variable and
;; return the result without simplification.
(define (basic-deriv exp var)
  (cond ((number? exp) 0)
        ((symbol? exp)
         (if (eq? exp var) 1 0))
        ((sum? exp)
         (list '+ (basic-deriv (left exp) var) (basic-deriv (right exp) var)))
        ((delta? exp)
         (list '- (basic-deriv (left exp) var) (basic-deriv (right exp) var)))
        ((product? exp)
         (list '+
          (list '* (left exp) (basic-deriv (right exp) var))
          (list '* (basic-deriv (left exp) var) (right exp))))
        ((sin? exp) (list '* (list 'cos (cadr exp)) (basic-deriv (cadr exp) var)))
        ((cos? exp) (list '* (list '* (list 'sin (cadr exp)) '-1) 
                          (basic-deriv (cadr exp) var)))
        ((expt? exp) 
         (cond ((eval? (left exp)) 0)
               ((and (symbol? (left exp)) (not (equal? (left exp) var))) 0)
               ((or (sum? (left exp)) (delta? (left exp))) 
                (list '* (list '* (list 'expt (left exp) (- (eval (right exp) ns) 1)) (right exp))
                      (basic-deriv (left exp) var)))
               ((product? (left exp))
                (cond ((equal? (left (left exp)) (right (left exp))) 
                       (basic-deriv (list 'expt (left (left exp)) (* (eval (right exp) ns) 2)) var))
                      ((or (number? (left (left exp))) (not (equal? (left (left exp)) var)))
                       (list '* (list 'expt (left (left exp)) (right exp)) 
                             (list '* (list 'expt (right (left exp)) (- (eval (right exp) ns) 1)) (right exp))))
                      ((or (number? (right (left exp))) (not (equal? (right (left exp)) var)))
                       (list '* (list '* (list 'pow (left (left exp)) (- (eval (right exp) ns) 1)) (right exp)) 
                             (list 'expt (right (left exp)) (right exp))))
                      (else (list '* (list 'expt (left exp) (- (right exp) 1)) (right exp)))))
               ((sin? (left exp)) (list '* (list '* (list 'expt (left exp) (- (eval (right exp) ns) 1)) (right exp)) 
                                        (basic-deriv (left exp) var)))
               ((cos? (left exp)) (list '* (list '* (list 'expt (left exp) (- (eval (right exp) ns) 1)) (right exp)) 
                                        (basic-deriv (left exp) var)))
               (else (list '* (list 'expt (left exp) (- (right exp) 1)) (right exp)))))
        (else (error "unknown expression type -- basic-deriv" exp))))

(define ns (make-base-namespace))
       
;; Predicates and access functions.

;; Test whether a expression is evaluatable.
(define (eval? exp)
  (cond ((and (number? exp) (not (symbol? exp))) #t)
        ((list? exp) (and (eval? (left exp)) (eval? (right exp))))
        (else #f)))

;; Test whether a list structure is a sum.
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; Test whether a list structure is a delta.
(define (delta? x)
  (and (pair? x) (eq? (car x) '-)))

;; Test whether a list structure is a product.
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; Test whether a list structure is a sin function.
(define (sin? x)
  (and (pair? x) (eq? (car x) 'sin)))

;; Test whether a list structure is a cos function.
(define (cos? x)
  (and (pair? x) (eq? (car x) 'cos)))

;; Test whether a list structure is a expt function.
(define (expt? x)
  (and (pair? x) (eq? (car x) 'expt)))

;; Get the left hand part of a sum, delta, product, or expt function.
(define (left exp)
  (cadr exp))

;; Get the right hand part of a sum, delta, product, or expt function.
(define (right exp)
  (caddr exp))

;; Basic simplification function 
;; (nothing too fancy ... doesn't know about commutativity or associativity).
(define (simplify exp)
  (cond ((sum? exp) (simplify-sum exp))
        ((delta? exp) (simplify-delta exp))
        ((product? exp) (simplify-product exp))
        ((sin? exp) (simplify-sin exp))
        ((cos? exp) (simplify-cos exp))
        ((expt? exp) (simplify-expt exp))
        ;; if we get here, we can't simplify exp
        (else exp)))

;; Simplify sum expression.
(define (simplify-sum exp)
  ;; recursively simplify the left and right parts
  (let ((a (simplify (left exp)))
        (b (simplify (right exp))))
    (cond ((equal? 0 a) b)
          ((equal? 0 b) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b)))))

;; Simplify delta expression.
(define (simplify-delta exp)
  (let ((a (simplify (left exp)))
        (b (simplify (right exp))))
    (cond ((equal? 0 a) (* b -1))
          ((equal? 0 b) a)
          ((and (number? a) (number? b)) (- a b))
          (else (list '- a b)))))

;; Simplify product expression.
(define (simplify-product exp) 
  (let ((a (simplify (left exp)))
        (b (simplify (right exp))))
    (cond ((or (equal? 0 a) (equal? 0 b)) 0)
          ((equal? 1 a) b)
          ((equal? 1 b) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b)))))

;; Simplify sin expression.
(define (simplify-sin exp)
  (let ((a (simplify (cadr exp))))
    (cond ((number? a) (sin a))
          (else (list 'sin a)))))

;; Simplify cos expression.
(define (simplify-cos exp)
  (let ((a (simplify (cadr exp))))
    (cond ((number? a) (cos a))
          (else (list 'cos a)))))

;; Simplify expt expression.
(define (simplify-expt exp)
  (let ((a (simplify (left exp)))
        (b (simplify (right exp))))
    (cond ((equal? b 0) 1)
          ((equal? a 0) 0)
          ((equal? b 1) a)
          ((and (number? a) (number? b)) (expt a b))
          (else exp))))

;; Unit tests.
(require rackunit)

(define deriv-tests 
  (test-suite 
   "tests for deriv program"
   (check-equal? (deriv 'x 'x) 1 "deriv of x wrt x")
   (check-equal? (deriv 'y 'x) 0 "deriv of y wrt x")
   (check-equal? (deriv '(+ x 3) 'x) 1 "deriv of (+ x 3) wrt x")
   (check-equal? (deriv '(* (+ 2 3) x) 'x) 5 "deriv of unsimplified expression")
   (check-equal? (deriv '(+ x y) 'x) 1 "deriv of (+ x y) wrt x")
   (check-equal? (deriv '(sin 0) 'x) 0 "deriv of (sin 0) wrt x")
   (check-equal? (deriv '(sin x) 'x) '(cos x) "deriv of (sin y) wrt x")
   (check-equal? (deriv '(sin y) 'x) 0 "deriv of (sin y) wrt x")
   (check-equal? (deriv '(cos 0) 'x) 0 "deriv of (cos 0) wrt x")
   (check-equal? (deriv '(cos x) 'x) '(* (sin x) -1) "deriv of (cos x) wrt x")
   (check-equal? (deriv '(cos y) 'x) 0 "deriv of (cos y) wrt x")
   (check-equal? (deriv '(expt 5 2) 'x) 0 "deriv of (expt 5 2) wrt x")
   (check-equal? (deriv '(expt (* (* (- 65 27) (+ 4 3)) 9) (- 5 3)) 'x) 0 
                        "deriv of (expt (* (* (- 65 27) (+ 4 3)) 9) (- 5 3)) wrt x")
   (check-equal? (deriv '(expt y 3) 'x) 0 "deriv of  (expt y 3) wrt x")
   (check-equal? (deriv '(sin (+ x y)) 'x) '(cos (+ x y)) "deriv of (sin (+ x y) wrt x")
   (check-equal? (deriv '(expt (+ x 3) 3) 'x) '(* (expt (+ x 3) 2) 3) 
                 "deriv of (expt (+ x 3) 3) wrt x")
   (check-equal? (deriv '(cos (- y x)) 'x) '(* (* (sin (- y x)) -1) -1)
                 "deriv of (cos (- y x)) wrt x")
   (check-equal? (deriv '(* (expt x 2) (sin x)) 'x) '(+ (* (expt x 2) (cos x)) (* (* x 2) (sin x)))
                 "deriv of (* (expt x 2) (sin x)) 'x) wrt x")
   (check-equal? (deriv '(expt (cos x) 2) 'x) '(* (* (cos x) 2) (* (sin x) -1))
                 "deriv of (expt (cos x) 2) wrt x")
   (check-equal? (deriv '(sin (+ (expt x 3) (* x 2))) 'x) 
                 '(* (cos (+ (expt x 3) (* x 2))) (+ (* (expt x 2) 3) 2)) 
                 "deriv of (sin (+ (expt x 3) (* x 2))) wrt x")
   (check-equal? (deriv '(- (+ x 1) (* x x)) 'x) '(- 1 (+ x x)) "deriv of (- (+ x 1) (* x x)) wrt x")
   (check-equal? (deriv '(* (+ x 1) (+ x -1)) 'x) '(+ (+ x 1) (+ x -1)) 
                 "deriv of (* (+ x 1) (+ x -1)) wrt x")
   (check-equal? (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))) "complex deriv")
   (check-equal? (deriv '(sin (+ (* x x) (* x 3))) 'x)
                 '(* (cos (+ (* x x) (* x 3))) (+ (+ x x) 3)) 
                 "deriv of (sin (+ (* x x) (* x 3))) wrt x")
   (check-equal? (deriv '(cos (- (* (* x x) x) (* x 10))) 'x)
                 '(* (* (sin (- (* (* x x) x) (* x 10))) -1) (- (+ (* x x) (* (+ x x) x)) 10))
                 "deriv of (cos (- (* (* x x) x) (* x 10))) wrt x")
   (check-equal? (deriv '(cos (* (expt x 3) y)) 'x) 
                 '(* (* (sin (* (expt x 3) y)) -1) (* (* (expt x 2) 3) y)) 
                 "deriv of (cos (* (expt x 3) y)) wrt x")
   ))

(require rackunit/text-ui)
;; Run the tests.
(run-tests deriv-tests)