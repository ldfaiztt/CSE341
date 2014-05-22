;;
;; CSE 341 -- Racket structs
;;
;; For more information see http://docs.racket-lang.org/guide/define-struct.html

#lang racket


(define-struct point (x y))

;; we now have accessors and tests defined:
(define p (make-point 10 10))

;; try these:
;; (point? p)
;; (point-x p)
;; (point-y p)
;; (point-y p)

(define (dist p1 p2)
  (let ((dx (- (point-x p1) (point-x p2)))
        (dy (- (point-y p1) (point-y p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define a (make-point 0 0))
(define b (make-point 3 4))

;; (dist a b)

;; a struct is different from the other datatypes in Racket:
(define (what-is-it v)
  (cond ((number? v) 1)
        ((procedure? v) 2)
        ((cons? v) 3)
        ((string? v) 4)
        ((symbol? v) 5)
        ((null? v) 6)
        (else 7)))

;; points as defined above are opaque (try printing one)
;; add a keyword transparent to show the fields: 

(define-struct tpoint (x y) #:transparent)

;; there are many other options for structs -- another declares
;; that all fields are mutable:

(define-struct mpoint (x y) #:mutable)

(define m (make-mpoint 10 10))

;; mpoints have all of the functions as points, plus setters:

;; (set-mpoint-x! m 100)
;; (set-mpoint-y! m 200)

