;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: tcase.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Test fixtures for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)


(defclass test ()
  ())

(defclass test-case (test)
  ((existing-suites :initform nil :accessor existing-suites
		    :allocation :class)
   (method-body
    :initarg :method-body :accessor method-body :initform nil
    :documentation
    "A function designator which will be applied to this instance
to perform that test-case.")
   (name :initarg :name :reader name :initform ""
	 :documentation "The name of this test-case, used in reports.")
   (description :initarg :description :reader description
		:documentation
		"Short description of this test-case, uses in reports")
   (suite :initform nil :accessor suite :initarg :suite))
  (:documentation
   "Base class for test-cases."))

(defmethod initialize-instance :after ((ob test-case) &rest initargs)
  (declare (ignore initargs))
  (if (null (existing-suites ob))
    (setf (existing-suites ob) (make-hash-table)))  ;;hash singleton
  (unless (gethash (type-of ob) (existing-suites ob))
    (setf (gethash (type-of ob) (existing-suites ob))
          (make-instance 'test-suite)))             ;;specifi suite singleton
  (setf (suite ob) (gethash (type-of ob) (existing-suites ob))))
 

(defgeneric set-up (test)
  (:documentation
   "Method called before performing a test, should set up the
environment the test-case needs to operate in."))

(defmethod set-up ((test test-case))
  )

(defgeneric tear-down (test)
  (:documentation
   "Method called after performing a test.  Should reverse everything
that the setup method did for this instance."))

(defmethod tear-down ((test test-case))
  )

(defmethod run ((ob test) &key (handle-errors t))
  "Generalized to work on test-case and test-suites"
  (let ((res (make-test-results)))
    (run-on-test-results ob res :handle-errors handle-errors)
    res))

(defmethod run-on-test-results ((test test-case) result
				&key (handle-errors t))
  (start-test test result)
  (run-protected test result :handle-errors handle-errors)
  (end-test test result))

(defmethod run-base ((test test-case))
  (set-up test)
  (unwind-protect
      (run-test test)
    (tear-down test)))

(defmethod run-test ((test test-case))
    (funcall (method-body test)))

(defmethod run-protected ((test test-case) res &key (handle-errors t))
  (if handle-errors
      (handler-case
	  (run-base test)
	(assertion-failed (condition)
	  (add-failure res test condition))
	(serious-condition (condition)
	  (add-error res test condition)))
      (run-base test))
  res)
