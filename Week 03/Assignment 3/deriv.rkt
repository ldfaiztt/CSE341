#lang racket

;;; CSE 341 - Racket
;;; A classic example of Scheme/Racket programs as data: symbolic differentiation.  Adapted from 
;;; "Structure and Interpretation of Computer Programs" Chapter 2.  Somewhat modified: doesn't use
;;; constructor functions, and it separates out finding the derivative from simplifying expressions.   
;;; Includes unit tests.

;; the top-level function deriv takes an expression and a variable, 
;; and returns the derivative of that expression with respect to the variable,
;; in simplified form
(define (deriv exp var)
  (simplify (basic-deriv exp var)))

;; basic-deriv takes the derivative of an expression exp with respect to a variable and
;; return the result without simplification
(define (basic-deriv exp var)
  (cond ((number? exp) 0)
        ((symbol? exp)
         (if (eq? exp var) 1 0))
        ((sum? exp)
         (list '+ (basic-deriv (left exp) var) (basic-deriv (right exp) var)))
        ((product? exp)
         (list '+
          (list '* (left exp) (basic-deriv (right exp) var))
          (list '* (basic-deriv (left exp) var) (right exp))))
        (else (error "unknown expression type -- basic-deriv" exp))))

;; predicates and access functions

;; test whether a list structure is a sum
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;; test whether a list structure is a product
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; get the left hand part of a sum or product
(define (left exp)
  (cadr exp))
;; get the right hand part of a sum or product
(define (right exp)
  (caddr exp))

;; basic simplification function (nothing too fancy ... doesn't know about commutativity or associativity)
(define (simplify exp)
  (cond ((sum? exp) (simplify-sum exp))
        ((product? exp) (simplify-product exp))
        ;; if we get here, we can't simplify exp
        (else exp)))

(define (simplify-sum exp)
  ;; to simplify a sum, we need to first recursively simplify the left and right parts
  (let ((a (simplify (left exp)))
        (b (simplify (right exp))))
    (cond ((equal? 0 a) b)
          ((equal? 0 b) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b)))))

(define (simplify-product exp) 
  (let ((a (simplify (left exp)))
        (b (simplify (right exp))))
    (cond ((or (equal? 0 a) (equal? 0 b)) 0)
          ((equal? 1 a) b)
          ((equal? 1 b) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b)))))
           
;; Unit tests.  See
;; http://docs.racket-lang.org/rackunit/quick-start.html
;; for documentation on Racket's unit testing library.
(require rackunit)

(define deriv-tests 
  (test-suite 
   "tests for deriv program"
   (check-equal? (deriv 'x 'x) 1 "deriv of x wrt x")
   (check-equal? (deriv 'y 'x) 0 "deriv of y wrt x")
   (check-equal? (deriv '(+ x 3) 'x) 1 "deriv of (+ x 3) wrt x")
   (check-equal? (deriv '(* (+ 2 3) x) 'x) 5 "deriv of unsimplified expression")
   (check-equal? (deriv '(+ x y) 'x) 1 "deriv of (+ x y) wrt x")
   ;; simplification is not as clever as it could be in the following case:
   (check-equal? (deriv '(* (+ x 1) (+ x -1)) 'x) '(+ (+ x 1) (+ x -1)) "deriv of (* (+ x 1) (+ x -1)) wrt x")
   (check-equal? (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))) "complex deriv")
   ))

(require rackunit/text-ui)
;; this line runs the tests ....
(run-tests deriv-tests)
