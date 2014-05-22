#lang racket
;; CSE341, Autumn 2012, Homework 4 Provided Code

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it's what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

;; Problem 2

;; Provided: lookup a variable in an environment.
;; Environments are stored as association lists (lists of name/value pairs).
;; Note that each pair is a single cons cell, whose car is the name and whose cdr is the value.
;; For example here is an environment that binds x to the MUPL integer 5:
;;    (list (cons "x" (int 5)))
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (caar env) str) (cdar env)]
        [#t (envlookup (cdr env) str)]))

;; the interpreter
(define (eval-prog p)
  ("CHANGE (to call eval-in-env appropriately)"))

;; helper function for eval -- evaluate an expression p in the environment env
(define (eval-in-env env p)
  (cond [(add? p) 
         (let ([v1 (eval-in-env env (add-e1 p))]
               [v2 (eval-in-env env (add-e2 p))])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE you need to add cases to this cond for each kind of 
        ;; MUPL expression
        [#t (error "bad MUPL expression")]))


;; Problem 3

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
