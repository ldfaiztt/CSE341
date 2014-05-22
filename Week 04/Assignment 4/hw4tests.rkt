#lang racket
;; starting file for unit tests for MUPL interpreter

;; replace the file name with the name of your own interpreter
(require "hw4solution.rkt")



(require rackunit)

(define mupl-tests 
  (test-suite 
   "tests for mupl"
   
   ;; integers should evaluate to themselves
   (check-equal? (eval-prog (int 5)) (int 5))
   
   ;; test that 3+4 equals 7
   (check-equal? (eval-prog (add (int 3) (int 4))) (int 7))
   
   ;; Check that 'add' checks the types of its arguments:
   ;; try adding 3 and aunit, and make sure it raises an exception.
   ;; The exception's message must contain the string "MUPL" -- if we
   ;; just checked that the call raises an exception this wouldn't be
   ;; enough, since the underlying Racket interpreter would also raise
   ;; an exception even if the MUPL interpreter didn't catch this.
   (check-exn #rx"MUPL" (lambda () (eval-prog (add (int 3) (aunit)))))
   
   ;; define and test a double function in MUPL
   (check-equal? 
    (eval-prog 
     (mlet "double" 
           (fun "double" "x" (add (var "x") (var "x")))
           (call (var "double") (int 10))))
    (int 20))
   
   ;; Define a range function in MUPL that takes two parameters lo and hi and returns
   ;; a list of integers between lo and hi inclusive.
   ;; Here's a range function in ordinary Racket:
   ;;   (define (range lo hi)
   ;;     (if (> lo hi) null (cons lo (range (+ 1 lo) hi))))
   ;; But in MUPL this needs to be curried.  Racket has syntax for this:
   ;;   (define ((range lo) hi)
   ;;     (if (> lo hi) null (cons lo ((range (+ 1 lo)) hi))))
   ;; However, let's write it out to avoid obscuring what's going on (still in Racket):
   ;;   (define (range lo)
   ;;     (lambda (hi) (if (> lo hi) null (cons lo ((range (+ 1 lo)) hi)))))
   ;; then ((range 5) 8) evaluates to (5 6 7 8)
   ;; OK, finally here it is in MUPL and as a unit test.
   ;; This is written out without shortcuts to make it clearer what's going on (maybe???)
   (check-equal? 
    (eval-prog 
     (mlet "range" 
           (fun "range" "lo"
                (fun #f "hi"
                     (ifgreater (var "lo") (var "hi")
                                (aunit)
                                (apair (var "lo") (call (call (var "range") (add (int 1) (var "lo"))) (var "hi"))))))
           (call (call (var "range") (int 5)) (int 8))))
    (apair (int 5) (apair (int 6) (apair (int 7) (apair (int 8) (aunit))))) ; the list (5 6 7 8) in MUPL
    )
   ;; ps - in Haskell this would just be:
   ;;    range lo hi = [lo..hi]
   ;; (couldn't resist ...)                 
   
   ;; test the mupl-mapAddN function -- add 7 to each element of the list (3 4 9)
   (check-equal? (eval-prog (call (call mupl-mapAddN (int 7))
                                  (racketlist->mupllist (list (int 3) (int 4) (int 9)))))
                 (racketlist->mupllist (list (int 10) (int 11) (int 16))))

   ))

(require rackunit/text-ui)
;; this line runs the tests ....
(run-tests mupl-tests)
