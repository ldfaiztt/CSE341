#lang racket
;;; Chun-Wei Chen
;;; CSE 341
;;; Assignment 4
;;; 10/26/12

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

;; convert a Racket list to a correct form of MUPL list.
(define (racketlist->mupllist rlist)
  (cond ((not (list? rlist)) (error "This function can only be applied to list"))
        ((null? rlist) (aunit))
        ((null? (cdr rlist)) (apair (car rlist) (aunit)))
        (else (apair (car rlist) (racketlist->mupllist (cdr rlist))))))

;; convert a MUPL list to a correct form of Racket list.
(define (mupllist->racketlist mlist)
  (cond ((aunit? mlist) null)
        ((not (apair? mlist)) (error "not a MUPL list")) 
        (else (append (list (apair-e1 mlist)) (mupllist->racketlist (apair-e2 mlist))))))

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
  (cond [(add? p) (eval-in-env '() p)]
        [(int? p) (eval-in-env '() p)]
        [(var? p) (eval-in-env '() p)]
        [(apair? p) (eval-in-env '() p)]
        [(aunit? p) p]
        [(isaunit? p) (eval-in-env '() p)]
        [(fst? p) (eval-in-env '() p)]
        [(snd? p) (eval-in-env '() p)]
        [(ifgreater? p) (eval-in-env '() p)]
        [(mlet? p) (eval-in-env '() p)]
        [(call? p) (eval-in-env '() p)]
        [(closure? p) p]
        [#t (error "bad MUPL expression")]))

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
        [(int? p) p]
        [(var? p) (envlookup env (var-string p))]
        [(apair? p)
         (let ([v1 (eval-in-env env (apair-e1 p))]
               [v2 (eval-in-env env (apair-e2 p))])
          (apair v1 v2))]
        [(aunit? p) p]
        [(isaunit? p) (if (aunit? (eval-in-env env (isaunit-e p))) (int 1) (int 0))]
        [(fst? p)
         (let ([v (eval-in-env env (fst-e p))])
           (if (apair? v) (apair-e1 v) (error "bad MUPL expression")))]
        [(snd? p)
         (let ([v (eval-in-env env (snd-e p))])
           (if (apair? v) (apair-e2 v) (error "bad MUPL expression")))]
        [(ifgreater? p)
         (let ([v1 (eval-in-env env (ifgreater-e1 p))]
               [v2 (eval-in-env env (ifgreater-e2 p))])
           (if (> (int-num v1) (int-num v2))
               (eval-in-env env (ifgreater-e3 p))
               (eval-in-env env (ifgreater-e4 p))))]
        [(mlet? p)
         (let ([v (eval-in-env env (mlet-e p))]) 
           (eval-in-env (append (list (cons (mlet-var p) v)) env) (mlet-body p)))]
        [(fun? p) (closure env p)]
        [(call? p) 
         (let ([exp1 (eval-in-env env (call-funexp p))]
               [v1 (eval-in-env env (call-actual p))])
           (cond [(not (closure? exp1)) (error "bad MUPL expression")]
                 [(equal? (fun-nameopt (closure-fun exp1)) #f) 
                  (let ([newenv1 (append (list (cons (fun-formal (closure-fun exp1)) v1)) (closure-env exp1))])
                    (eval-in-env newenv1 (fun-body (closure-fun exp1))))]
                 [#t (let ([newenv2 (append (list (cons (fun-formal (closure-fun exp1)) v1))
                                            (list (cons (fun-nameopt (closure-fun exp1)) exp1)) (closure-env exp1))])
                       (eval-in-env newenv2 (fun-body (closure-fun exp1))))]))]
        [(closure? p) p]
        [#t (error "bad MUPL expression")]))


;; Problem 3

;; returns a mupl expression that evaluates e1 and if the result is unit 
;; then it evaluates e2 when run, else it evaluates e3.
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

;; takes a Racket list (list of variables/values pairs) and a MUPL expression, 
;; and then convert it into a nested mlet expression. 
(define (mlet* lstlst e2)
  (if (null? lstlst) 
      e2 
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

;; return a MUPL expression that evaluates e3 when e1 and e2 are equal when run,
;; else it evaluates e4.
(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1 
        (mlet "_y" e2 
              (ifgreater (var "_x") (var "_y") e4 
                         (ifgreater (var "_y") (var "_x") e4 e3))))) 

;; Problem 4

;; a mupl function that acts like map
(define mupl-map (fun "map" "f" 
                      (fun "fbody" "list"
                           (ifgreater (isaunit (var "list")) (int 0)
                                      (aunit)
                                      (mlet "first" (call (var "f") (fst (var "list")))
                                            (mlet "rest" (call (var "fbody") (snd (var "list")))
                                                  (apair (var "first") (var "rest"))))))))

;; a mupl function that takes a mupl integer i and returns a mupl function that 
;; takes a list of mupl integers and returns a new list that adds i to
;; every element of the list.
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "N" (call (var "map") (fun #f "element" (add (var "N") (var "element")))))))