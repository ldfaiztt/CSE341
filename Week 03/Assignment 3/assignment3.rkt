#lang racket
;;; Chun-Wei Chen
;;; CSE 341
;;; Assignment 3 - Racket Warmup
;;; 10/17/12

;; This function takes a list of numbers, and returns 
;; a new list of the squares of those numbers.
(define (squares numList)
  (if (null? numList)
      numList
      (cons (* (car numList) (car numList)) (squares (cdr numList)))))

;; This function takes a list of numbers, and returns 
;; a new list of the squares of those numbers.
(define (map-squares numList)
  (if (null? numList)
      numList
      (map (lambda (x) (* x x)) numList)))

;; This function takes a list of integers and 
;; tests whether it's in strict ascending order.
(define (ascending intList)
  (cond ((null? intList) #t)
        ((null? (cdr intList)) #t)
        ((>= (car intList) (cadr intList)) #f)
        (else (ascending (cdr intList)))))

;; This function takes a list of let* expression and 
;; convert it into equivalent nested let expressions.
(define (let*->let let*Expr)
  (cond ((null? (cadr let*Expr)) (caddr let*Expr))
        (else (insert-let (cadr let*Expr) (caddr let*Expr)))))

;; This is a helper function of let*->let function.
;; It helps to insert let into the expression.
(define (insert-let letList last)
  (cond ((null? (cdr letList)) (list 'let (list (car letList)) last))
        (else (list 'let (list (car letList)) (insert-let (cdr letList) last)))))

;; Unit tests.
(require rackunit)

;; Test squares function.
(define squares-tests 
  (test-suite 
   "tests for squares function"
   (check-equal? (squares '()) '() "squares of empty list")
   (check-equal? (squares '(1)) '(1) "squares of list of one number")
   (check-equal? (squares '(1 2 3)) '(1 4 9) "squares of list of some numbers")
   (check-equal? (squares '(3/2 32.0 -100)) '(9/4 1024.0 10000) 
                 "squares of list of different type of numbers")
   ))

;; Test map-squares function.
(define map-squares-tests 
  (test-suite 
   "tests for map-squares function"
   (check-equal? (map-squares '()) '() "map-squares of empty list")
   (check-equal? (map-squares '(1)) '(1) "map-squares of list of one number")
   (check-equal? (map-squares '(1 2 3)) '(1 4 9) 
                 "map-squares of list of some numbers")
   (check-equal? (map-squares '(3/2 32.0 -100)) '(9/4 1024.0 10000) 
                 "map-squares of list of different type of numbers")
   ))

;; Test ascending function.
(define ascending-tests
  (test-suite
   "tests for ascending function"
   (check-equal? (ascending '()) #t "ascending of empty list")
   (check-equal? (ascending '(0)) #t "ascending of list of one integer")
   (check-equal? (ascending '(1 2 3)) #t 
                 "list of some integers in ascending order")
   (check-equal? (ascending '(3 2 1)) #f 
                 "list of some integers not in ascending order")
   (check-equal? (ascending '(1 2 2 3)) #f
                 "list of some integers not in strict ascending order")
   ))

(define ns (make-base-namespace))
;; Test let*->let function.
(define let*->let-tests
  (test-suite
   "tests for let*->let function"
   (check-equal? (let*->let '(let* () (+ x 1))) '(+ x 1) "no variable bound to let*") 
   (check-equal? (let*->let '(let* ((x 3) (y 4)) (+ x y))) 
                            '(let ((x 3)) (let ((y 4)) (+ x y))) 
                            "two variable2 bound to let*")
   (check-equal? (let*->let '(let* ((x 3) (y (+ x 1)) (z (+ x y))) (+ x y z)))
                            '(let ((x 3)) (let ((y (+ x 1))) (let ((z (+ x y))) (+ x y z))))
                            "three variables bound to let*")
   (check-equal? (eval '(let* ((x 3) (y (+ x 1))) (+ x y)) ns) 
                 (eval (let*->let '(let* ((x 3) (y (+ x 1))) (+ x y))) ns))
  ))

(require rackunit/text-ui)
;; Run all the tests.
(run-tests squares-tests)
(run-tests map-squares-tests)
(run-tests ascending-tests)
(run-tests let*->let-tests)

   