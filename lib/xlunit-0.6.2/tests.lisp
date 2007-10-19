;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Id:      $Id: tests.lisp 8702 2004-03-08 01:41:52Z kevin $
;;;; Purpose: Self Test suite for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:xlunit-tests
  (:use #:cl #:xlunit)
  (:export #:do-tests))
(in-package #:xlunit-tests)

(define-condition test-condition (error)
  ())


;; Helper test fixture

(defclass was-run (test-case)
  ((log :accessor ws-log)))

(defmethod set-up ((self was-run))
    (setf (ws-log self) "setup "))

(defmethod tear-down ((self was-run))
  (setf (ws-log self)
	(concatenate 'string (ws-log self) "teardown ")))

(def-test-method test-method ((self was-run) :run nil)
    (setf (ws-log self) 
      (concatenate 'string (ws-log self) "test-method ")))

(def-test-method test-broken-method ((self was-run) :run nil)
    (assert-equal pi (/ 22 7)))

(def-test-method test-not-eql ((self was-run) :run nil)
    (assert-not-eql (cons t t) (cons t t)))

(def-test-method test-eql ((self was-run) :run nil)
    (let ((obj (cons t t)))
      (assert-eql obj obj)))

(def-test-method test-error-method ((self was-run) :run nil)
    (error "Err"))

(def-test-method test-condition-without-cond ((self was-run) :run nil)
  (assert-condition 'error (list 'no-error)))

#+ignore
(def-test-method test-not-condition-with-cond ((self was-run) :run nil)
  (assert-not-condition 'test-condition 
			(signal 'test-condition)))


;;; Second helper test case

(defclass test-two-cases (test-case)
  ())

(def-test-method test-1 ((self test-two-cases) :run nil)
    (assert-true t))

(def-test-method test-2 ((self test-two-cases) :run nil)
    (assert-false nil))

;;; Main test fixture

(defclass test-case-test (test-case)
  ())


(def-test-method test-template-method ((self test-case-test) :run nil)
  (let ((test (named-test 'test-method (get-suite was-run))))
    (run test)
    (assert-equal (ws-log test) "setup test-method teardown ")))

(def-test-method test-results ((self test-case-test) :run nil)
  (assert-equal "1 run, 0 erred, 0 failed" 
		(summary (run (named-test 'test-method 
					  (get-suite was-run))))))

(def-test-method test-eql ((self test-case-test) :run nil)
  (assert-equal "1 run, 0 erred, 0 failed" 
		(summary (run (named-test 'test-eql (get-suite was-run))))))

(def-test-method test-not-eql ((self test-case-test) :run nil)
  (assert-equal "1 run, 0 erred, 0 failed" 
		(summary (run (named-test 'test-not-eql
					  (get-suite was-run))))))

(def-test-method test-fn ((self test-case-test) :run nil)
  (let ((test (make-instance 'test-case :name 'test-fn
			      :method-body
			      (lambda () 
				(declare (ignore test))
				(assert-equal 10 10)))))
    (assert-equal "1 run, 0 erred, 0 failed"
		  (summary (run test)))))

(def-test-method test-failed-result ((self test-case-test) :run nil)
  (assert-equal "1 run, 0 erred, 1 failed"
		(summary (run
			  (named-test 'test-broken-method
				      (get-suite was-run))))))

(def-test-method test-error-result ((self test-case-test) :run nil)
    (assert-equal "1 run, 1 erred, 0 failed"
		  (summary (run
			    (named-test 'test-error-method
					(get-suite was-run))))))
  
(def-test-method test-suite ((self test-case-test) :run nil)
  (let ((suite (make-instance 'test-suite))
	(result (make-test-results)))
    (add-test suite (named-test 'test-method (get-suite was-run)))
    (add-test suite (named-test 'test-broken-method (get-suite was-run)))
    (run-on-test-results suite result)
    (assert-equal "2 run, 0 erred, 1 failed" (summary result))))

(def-test-method test-dynamic-suite ((self test-case-test) :run nil)
  (assert-equal "2 run, 0 erred, 0 failed" 
		(summary (run (get-suite test-two-cases)))))

(def-test-method test-condition ((self test-case-test) :run nil)
  (assert-condition 
   'test-condition 
   (error 'test-condition)))

(def-test-method test-condition-without-cond ((self test-case-test) 
					      :run nil)
  (assert-equal "1 run, 0 erred, 1 failed"
		(summary (run
			  (named-test 'test-condition-without-cond
				      (get-suite was-run))))))

#+ignore
(def-test-method test-not-condition ((self test-case-test) :run nil)
  (assert-not-condition 
   'test-condition 
   (progn)))

#+ignore
(def-test-method test-not-condition-with-cond ((self test-case-test) 
					      :run nil)
  (assert-equal "1 run, 0 erred, 1 failed"
		(summary (run
			  (named-test 'test-not-condition-with-cond
				      (get-suite was-run))))))
		
#+ignore    
(textui-test-run (get-suite test-case-test))


(defun do-tests ()
  (or (was-successful (run (get-suite test-case-test)))
      (error "Failed tests")))
