;;; CSE 341 -- Racket Basics
;;; Code to accompany lecture notes

#lang racket


;; reverse a list and double it

;; less efficient version:
(define (r2 x) 
  (append (reverse x) (reverse x)))

;; more efficient version:
(define (better-r2 x) 
  (let ((r (reverse x)))
    (append r r)))


;; let* example (turned into a function)
(define (let*test)
  (let* ((x 3)          
         (y (+ x 1)))
    (+ x y)))

;; illustratiion of the environment for finding the values of the bound variables in a let
(define (flip x y)
  (let ((x (+ y 1))
        (y (- x 10)))
    (+ x y)))

;; nested lets
(define (way-too-many-lets x y z)
  (let ((x (+ x y))
        (y (- x y)))
    (let ((x (* x 2))
          (y (* x y 10)))
      (+ x y z))))

(define square-diff  
  (lambda (x1 x2)
    (* (- x1 x2) (- x1 x2))))

(define (double x)
  (* 2 x))

(define (centigrade-to-fahrenheit c)
  (+ (* 1.8 c) 32.0))

;; to try: (centigrade-to-fahrenheit 100.0) 

;; showing global and local variables
(define x 10)

(define (add1 x)
  (+ x 1))

(define (double-add x)
  (double (add1 x)))

;; try: (double-add x) 

;; Functions can take 0 arguments: 

(define (test) 3)
;; try: (test)  

(define not-a-function 3)
;; try: not-a-function   
;; (not-a-function) 


;; List structure examples

(define clam '(1 2 3))
(define octopus clam)              ; clam and octopus refer to the same list


;; to try:
; (eq? 'clam 'clam)         
; (eq? clam clam)              
; (eq? clam octopus)
; (eq? clam '(1 2 3))           
; (eq? '(1 2 3) '(1 2 3))  
; (eq? 10 10)
; (eq? (/ 1.0 3.0) (/ 1.0 3.0)) 
; (eqv? 10 10)
; (eqv? 10.0 10.0)
; (eqv? 10.0 10) 
; (equal? clam '(1 2 3)) 
; (equal? '(1 2 3) '(1 2 3)) 
 

(define (single-digit x)
  (and (> x 0) (< x 10)))

;; Conditional examples

(define (my-max x y)     
  (if (> x y) 
      x 
      y))    

;; to try: (my-max 10 20)                  

(define (my-max3 x y z)
   (if (and (> x y) (> x z))
       x
       (if (> y z) 
           y
           z)))  

(define (sum xs)
  (if (null? xs) 
      0
      (+ (car xs) (sum (cdr xs)))))

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

;; cond -- a more general conditional 

(define (sign n)
  (cond ((> n 0) 1)
        ((< n 0) -1)
        (else 0)))

;; Tail Recursion examples


(define (all-positive x)
  (cond ((null? x) #t)
        ((<= (car x) 0) #f)
        (else (all-positive (cdr x)))))
;; to try: (all-positive '(3 5 6))  
;;         (all-positive '(3 5 -6)) 

(define (my-member e x)
  (cond ((null? x) #f)
        ((equal? e (car x)) #t)
        (else (my-member e (cdr x)))))



;; Using Accumulators to Make a Function Tail-recursive

(define (std-factorial n)
  (if (zero? n)
      1
      (* n (std-factorial (- n 1)))))

;; Here is a version that is tail recursive:

(define (factorial n)
  (acc-factorial n 1))

;; auxiliary function that takes an additional parameter (the accumulator,
;; i.e. the result computed so far)
(define (acc-factorial n sofar)
  (if (zero? n)
      sofar
      (acc-factorial (- n 1) (* sofar n))))

;; Higher-Order Functions

;; examples to try:
;  (map null? '(3 () () 5))
;  (map round '(3.3 4.6 5.9)) 
;  (map cdr '((1 2) (3 4) (5 6)))
;  (map (lambda (x) (* 2 x)) '(3 4 5))
;  (filter (lambda (n) (> n 10)) '(5 10 15 20))

(define (add-n-to-list alist n)
  (map (lambda (x) (+ n x)) alist))
;;Note that in the add-n-to-list function, the body of the lambda function
;; can refer to the variable n, which is in the lexical scope of the lambda
;; expression.

;; map can also be used with functions that take more than one argument.

;; to try:
;  (map + '(1 2 3) '(10 11 12))
;  (map (lambda (x y) (list x y)) '(a b c) '(j k l))

;; Defining higher order functions

(define (my-map f s)
  (if (null? s)
      '()
      (cons (f (car s)) (my-map f (cdr s)))))

;; return a new	list that consists of all elements of s	for which f is true
(define (my-filter f s)
  (cond ((null? s) '())
        ((f (car s)) (cons (car s) (my-filter f (cdr s))))
        (else (my-filter f (cdr s)))))



;; Nested scope examples (scopes within scopes within scopes ....)

(define (anemone x)
  (+ x 1))
;; try 

(anemone 10)

(define (anemone-test)
  (let ((anemone (lambda (x) (* x 2))))
    (anemone 10)))

;; try
(anemone-test)
;; within the let, anemone is rebound to a different function.