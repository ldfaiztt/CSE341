;; CSE 341 Autumn 2012 - Racket Macros


#lang racket

;; a cosmetic macro -- adds then, else
(define-syntax my-if             ; macro name
  (syntax-rules (then else)      ; literals it uses, if any
    ((my-if e1 then e2 else e3)  ; pattern
     (if e1 e2 e3))))            ; template

(my-if #t then 'a else 'b)
(my-if #f then 'a else 'b)
(my-if (= 1 (- 3 2)) then (car '((a b) c)) else (cdr '((a b) c)))

;; We can use the functions 'expand-once' and 'syntax->datum' to
;; see how macros are expanded:

;; (expand-once '(my-if #t then 'a else 'b))
;; (syntax->datum (expand-once '(my-if #t then 'a else 'b)))

;; 'expand' expands all the way instead

;; NB: macro pattern matching is matching source text at "compile time".
;; This is in contrast to pattern matching in Haskell or ML, which 
;; matches data structures at "run time".

;; A key use of macros: controlling evaluation order, 
;; AKA, you, too, can create special forms.
;;
;; Example:  "or"
;; (or a b) means if a is true, return a, otherwise return b
;; It's built-in, but what if it weren't?
(define (myor1 a b)
  (if a a b))

 (myor1 1 2)
 (myor1 #f 2)
 (myor1 (begin (display 'hi) 1) 
        (begin (display 'ho) 2))
 ;; oops - we evaluated both arguments to myor
 
 (define-syntax myor2
   (syntax-rules ()
     ((myor2 a b)
      (if a a b))))
 
 (myor2 1 2)
 (myor2 #f 2)
 (myor2 (begin (display 'hi) 1) 
        (begin (display 'ho) 2))
 ;; still oops -  we evauate the first argument twice if it isn't #f
 
 ;; this is closer, but doesn't match the Scheme semantics for or:
  (define-syntax myor3
   (syntax-rules ()
     ((myor3 a b)
      (if a #t b))))

;; (aside: personally I think it's bad style to make use of the specific
;; value that or returns -- just assume it's a boolean)

 
 (define-syntax myor4
    (syntax-rules ()
     ((myor4 a b)
      (let ((temp a))
        (if temp temp b)))))
 
 (myor4 1 2)
 (myor4 #f 2)
 (myor4 (begin (display 'hi) 1) 
        (begin (display 'ho) 2))
 (myor4 (begin (display 'hi) #f) 
        (begin (display 'ho) 2))
 (define temp 42)
 (myor4 (begin (display 'hi) #f) 
        (begin (display 'ho) (+ 2 temp)))

 ;; or actually takes an indefinite number of arguments
 (define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or e) e)
    ((my-or e1 e2 ...)
     (let ((temp e1))
       (if temp
           temp
           (my-or e2 ...))))))

;; see how this expands ...
;; (syntax->datum (expand-once '(my-or (= x 2) (= x 3) (= x 4))))

;; Notice that Scheme doesn't get confused about name conflicts,
;; even though we are using temp in the let (and it is also used
;; in the macro definition).  These are Scheme's 'hygenic macros'.
(let ((temp 5))
  (my-or (= temp 10) (= temp 5)))


(define-syntax my-list
  (syntax-rules ()
    ((list) ())
    ((list e1 e2 ...)
     (cons e1 (list e2 ...)))))

;; a bad-style macro -- a function is better
(define-syntax double 
  (syntax-rules ()
         ((double e)
          (* 2 e))))

;(define (double x) (* 2 x))

;; a worse macro -- effects repeated
(define-syntax double2 
  (syntax-rules ()
    ((double2 e)
     (+ e e))))

