;; CSE 341 - sample solution to Mini Exercise #2
;; structs, representing objects, lexical scoping, and macros.

#lang racket


;; Question 1 - point3d struct
(struct point3d (x y z)  #:transparent #:mutable)
(define p (point3d 0 0 0))
(set-point3d-z! p 10)
(display "Question 1 result: ")
(print p)
(newline) (newline)


;; Question 2 - simulated cell class
;; each simulated instance of cell has its own value

(define (make-cell)
  (let ((value null))
    
    (define (get-value) value)
    (define (set-value! n)
      (set! value n))
        
    (define (dispatch m)
      (cond ((eq? m 'get-value) get-value)
            ((eq? m 'set-value!) set-value!)
            (else (error "Unknown request -- MAKE-CELL"  m))))
    dispatch))

;; some expressions to try:
;; (define c (make-cell))
;; ((c 'get-value))
;; ((c 'set-value!) "octopus")
;; ((c 'get-value!))


;; Question 3 - simulated point class
;; each simulated instance of point has its own x and y

(define (make-point)
  (let ((x 0)
        (y 0))
    
    (define (get-x) x)
    (define (get-y) y)
    
    (define (set-x! new-x)
      (set! x new-x))
    
    (define (set-y! new-y)
      (set! y new-y))
    
    (define (print-point)
      (display "point x=")
      (display x)
      (display " y=")
      (display y))
    
    (define (dispatch m)
      (cond ((eq? m 'get-x) get-x)
            ((eq? m 'get-y) get-y)
            ((eq? m 'set-x!) set-x!)
            ((eq? m 'set-y!) set-y!)
            ((eq? m 'print-point) print-point)
            (else (error "Unknown request -- MAKE-POINT"  m))))
    dispatch))

;; some expressions to try:
;; (define q (make-point))
;; ((q 'get-x))
;; ((q 'set-x!) 100)
;; ((q 'print-point))

;; Question 4
(let ((x 2))
  (let ((f (lambda (n) (+ x n))))
    (let ((x 17))
      (f 3))))
;; this evaluates to 5
;; (f 3) is evaluated in an environment with f bound to 
;; the lambda, and x bound to 17 (overriding the x=2 binding)
;; the lambda captures its environment of definition, so when the
;; body is evaluated n is bound to 3, and x is bound to 2

;; Question 5
(define (addN n)
  (lambda (m) (+ m n)))

(let* ((m 10)
       (n 20)
       (addit (addN 3)))
  (addit 100))
;; the let* evaluates to 103
;; addit is bound to a lambda.  The lambda captures its environment of
;; definition, so that n=3.  Then we call (addit 100).  This evaluates 
;; the lambda with m bound to 100, so we get a value of 103.  The 
;; (m 10) and (n 20) bindings have no effect.

;; Question 6
;; this time we don't have the problems of possible multiple
;; evaluations that we have with "or"
(define-syntax and2
  (syntax-rules ()
    ((and2 a b)
     (if a b #f))))
;; examples
(and2 (> 1 10) (/ 1 0))
(and2 #t "squid")   
; paste this in the interaction pane to see how it expands:
; (syntax->datum (expand-once '(and2 (> 1 10) (/ 1 0))))

