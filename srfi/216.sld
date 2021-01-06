;; -*- mode: scheme; -*-
;; Time-stamp: <2021-01-06 22:40:26 lockywolf>
;; Author: lockywof
;; Created: <2020-11-03 Tue 16:23>
;; Title: r7rs library declarations for srfi-216


(define-library (srfi 216)
  (import (scheme base))
  (import (scheme time))
  (import (only (scheme write) display))
  (import (scheme lazy))
  (import (only (srfi 27) random-integer random-real))
  (import (only (srfi 18)
		thread-start!
		make-thread
		thread-join!
		make-mutex
		mutex-lock!
		mutex-unlock!))
  (export runtime ;; function
	  random ;; function
	  parallel-execute ;; function
	  test-and-set! ;; function
	  cons-stream ;; syntax
	  stream-null? ;; function
	  the-empty-stream ;; constant
	  true ;; constant
	  false ;; constant
	  nil ;; constant
	  )

  (include "216/216.scm"))

