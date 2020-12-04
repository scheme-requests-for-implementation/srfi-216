;; -*- mode: scheme; -*-
;; Time-stamp: <2020-12-04 12:10:58 lockywolf>
;; Title: Testing SRFI-?.
;; Author: lockywolf
;; Date: <2020-11-03 Tue>

;;; Manual
;; The tests use the standard simple srfi-78 testing framework.
;; Run the tests in the interpreter and see which fail. This does not work with tests that run indefinitely.


(import (srfi 216))
(import (srfi 78)) ;; provides (check ...)
(import (only (srfi 27) random-integer))
(import (only (scheme time) current-second))
(import (only (scheme write) display))

(define (sleep-a-little)
  (define starting-time (current-second))
  (let loop ()
    (if (< (- (current-second) starting-time) 1)
       (loop))))

;;; Test runtime.
(check (> (let* ((first-value (runtime))
                 (second-value (begin (sleep-a-little) (runtime))))
                 (- second-value first-value)) 0) => #t)

;;; Test random.
(check (> (random 100) -1) => #t)
(check (< (random 100) 100) => #t)

;;; Test parallel-execute.

;; Testing parallel programming is always hard.
;; In this case, please verify that your code produces both 2 _and_ 3.

(define (my-wait n)
  (if (= n 0)
      #t
      (my-wait (- n 1))))

(define testval 1)
(do ((i 0 (+ i 1)))
    ((= i 5) #f)
  (parallel-execute
   (lambda ()
     (my-wait (random-integer 100))
     (set! testval 2))
   (lambda ()
     (my-wait (random-integer 100))
     (set! testval 3)))
  (check (or (begin
	       (newline)
	       (display "Test value=")
	       (display testval)
	       (newline)
	       (= testval 2))
	     (begin
	       (newline)
	       (display "Test value=")
	       (display testval)
	       (newline)
	       (= testval 3))) => #t))

;; Testing test-and-set!

(define cell (list #f))
(check (test-and-set! cell) => #f)
(set-car! cell (list #t))
(check (test-and-set! cell) => #t)
; But how do we check atomicity?

;; Testing booleans.
(check (if false
	   #t
	   #f) => #f)
(check (if true
	   #t
	   #f) => #t)

(check nil => '())

;; Testing streams.


(check (stream-null? the-empty-stream) => #t)

(check (car (cons-stream 'a 'b)) => 'a)

(check (promise? (cdr (cons-stream 'a 'b))) => #t)

(check (force (cdr (cons-stream 'a 'b))) => 'b)

