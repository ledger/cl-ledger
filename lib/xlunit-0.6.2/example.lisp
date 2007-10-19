;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: example.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Example file for XLUnit
;;;;
;;;; *************************************************************************

(defpackage #:xlunit-example
  (:use #:cl #:xlunit)
  (:export #:math-test-suite))

(in-package #:xlunit-example)

;;; First we define some basic test-cases that we are going to need to
;;; perform our tests.  A test-case is a place to hold data we need
;;; during testing.  Often there are many test cases that use the same
;;; data.  Each of these test cases is an instance of a test-case.

(defclass math-test-case (test-case)
  ((numbera :accessor numbera)
   (numberb :accessor numberb))
  (:documentation "Test test-case for math testing"))

;;; Then we define a set-up method for the test-case.  This method is run
;;; prior to perfoming any test with an instance of this test-case.  It
;;; should perform all initialization needed, and assume that it is starting
;;; with a pristine environment, well to a point, use your head here.

(defmethod set-up ((tcase math-test-case))
  (setf (numbera tcase) 2)
  (setf (numberb tcase) 3))


(def-test-method test-addition ((test math-test-case) :run nil)
  (let ((result (+ (numbera test) (numberb test))))
    (assert-true (= result 5))))

(def-test-method test-subtraction ((test math-test-case) :run nil)
  (let ((result (- (numberb test) (numbera test))))
    (assert-equal result 1)))

;;; This method is meant to signal a failure
(def-test-method test-subtraction-2 ((test math-test-case) :run nil)
  (let ((result (- (numbera test) (numberb test))))
    (assert-equal result 1 "This is meant to failure")))

;;;; Finally we can run our test suite and see how it performs.
(textui-test-run (get-suite math-test-case))

