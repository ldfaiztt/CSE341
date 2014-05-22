;; CSE 341 - notes on recursive function definitions (for MUPL interpreter)

#lang racket

; In the assignment, the struct for functions is defined as:
; If s1 and s2 are Racket strings and e is a mupl expression, then (fun s1 s2 e) is a mupl expression (a function).
; In e, s1 is bound to the function itself (for recursion) and s2 is bound to the (one) argument. 

; Why (you might ask) do we have this strange s1 argument?  Racket doesn't have this???

; Here's a garden-variety recursive function:
(define (fact1 n)
  (if (= n 0)
      1
      (* n (fact1 (- n 1)))))

; We could also write this as
(define fact2 
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact2 (- n 1))))))

; Note what fact2 is bound to!

; Does this work?
(define fact3 "this is an impostor function!!!")
(let ((fact3 (lambda (n)
               (if (= n 0)
                   1
                   (* n (fact3 (- n 1)))))))
  (fact3 6))

; Aargh!  Wrong binding for the recursive call!  
; (Comment out the above definition if you want to run this code!!)


; Racket provides another version of let for this:
(letrec ((fact4 (lambda (n)
               (if (= n 0)
                   1
                   (* n (fact4 (- n 1)))))))
  (fact4 6))

; now this works

; The obvious way to implement letrec is to use side effects -- make a dummy version of the variables
; to be bound (e.g. fact4), bind them to the value 'undefined', and then set the variable after the
; corresponding expression is evaluated.  And this is what Racket does -- see
; http://docs.racket-lang.org/reference/let.html

; But we've been avoiding side effects in the MUPL interpreter, hence the hack to get around this issue
; for recursive function definitions.
