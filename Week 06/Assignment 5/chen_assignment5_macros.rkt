#lang racket
;;; Chun-Wei Chen
;;; CSE 341
;;; Assignment 5
;;; 11/05/12

;; Define a structure to hold a delayed expression.
;; is-evaluated is true if the expression has been evaluated already,
;; and false if not. Value is either a lambda (if is-evaluated is
;; false) or the resulting value (if is-evaluated is true).
(struct delay-holder (is-evaluated value) #:transparent #:mutable)

;; Make a delayed expression.
(define-syntax my-delay 
  (syntax-rules () 
    ((my-delay e) (delay-holder #f (lambda () e)))
    ((my-delay e1 e2) (delay-holder #f (lambda () e1 e2)))
    ((my-delay e1 e2 ...) (delay-holder #f (lambda () e1 e2 ...)))))

;; If the expression hasn't been evaluated, force it to be evaluated.
(define (my-force holder)
  (cond ((delay-holder-is-evaluated holder) (delay-holder-value holder))
        (else (set-delay-holder-is-evaluated! holder #t)
              (set-delay-holder-value! holder ((delay-holder-value holder)))
              (delay-holder-value holder))))
    
;; This works the same as built-in Racket and.
(define-syntax my-and 
  (syntax-rules () 
    ((my-and) #t)
    ((my-and e) e)
    ((my-and e1 e2 ...)
     (if e1 
         (my-and e2 ...)
         #f))))

;; Unit tests.
(require rackunit)

;; Test my-and function.
(define my-and-tests 
  (test-suite 
   "tests for my-and"
   (check-equal? (my-and) #t "0 expression")
   (check-equal? (my-and (> 2 3)) #f "1 expression")
   (check-equal? (my-and 1) 1 "1 expression")
   (check-equal? (my-and(< 5 4) (/ 1 0)) #f "2 expressions")
   (check-equal? (my-and 1 2) 2 "2 expressions")
   (check-equal? (my-and (my-and (> 3 2) 1) (my-and (< 4 5) 3)) 3 "2 expressions, nested and")
   (check-equal? (my-and (my-and (< 3 2) (/ 1 0)) (my-and (/ 1 0) (/ 1 0))) #f "2 expressions, nested and")
   (check-equal? (my-and 1 2 3) 3 "3 expressions")
   (check-equal? (my-and 1 (> 0 1) (/ 1 0)) #f "3 expressions")
   (check-equal? (my-and 1 2 3 4) 4 "4 expressions")
   (check-equal? (my-and (< 4 3) (/ 1 0) (/ 1 0) (/ 1 0)) #f "4 expressions")))

;; Test my-delay and my-force function.
(define my-delay&force-tests
  (test-suite
   "tests for my-delay function"
   (let ((counter 0))
     (let ((d (my-delay (begin (set! counter (+ counter 1)) counter))))
       (check-equal? counter 0 "counter starts at 0, not evaluated")
       (check-equal? (my-force d) 1 "first time force")
       (check-equal? (my-force d) 1 "second time force, the expression isn't getting evaluated again")
       (check-equal? (my-force d) 1 "third time force, the expression isn't getting evaluated again")
       (check-equal? (my-force d) 1 "fourth time force, the expression isn't getting evaluated again")))))

(require rackunit/text-ui)
;; Run all the tests.
(run-tests my-delay&force-tests)
(run-tests my-and-tests)