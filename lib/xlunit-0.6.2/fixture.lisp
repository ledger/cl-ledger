;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: fixture.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Test fixtures for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)


(defclass test-fixture ()
  ((test-fn
    :initarg :test-fn :reader test-fn :initform nil
    :documentation
    "A function designator which will be applied to this instance
to perform that test-case.")
   (test-name
    :initarg :test-name :reader test-name
    :documentation
    "The name of this test-case, used in reports.")
   (test-description
    :initarg :description :reader description
    :documentation
    "Short description of this test-case, uses in reports"))
  (:documentation
   "Base class for test-fixtures.  Test-cases are instances of test-fixtures."))

(defgeneric setup (test)
  (:documentation
   "Method called before performing a test, should set up the
environment the test-case needs to operate in."))

(defmethod setup ((test test-fixture))
  t)

(defgeneric teardown (test)
  (:documentation
   "Method called after performing a test.  Should reverse everything
that the setup method did for this instance."))

(defmethod teardown ((test test-fixture))
  t)


(defmacro handler-case-if (test form &body cases)
  `(if ,test
       (handler-case
        ,form
	,@cases)
     ,form))

(defmacro unwind-protect-if (test protected cleanup)
  `(if ,test
       (unwind-protect
	   ,protected
	 ,cleanup)
     (progn ,protected ,cleanup)))


(defmethod run-test ((test test-fixture)
		     &key (result (make-instance 'test-result))
		     (handle-errors t))
  "Perform the test represented by the given test-case or test-suite.
Returns a test-result object."
  (incf (test-count result))
  (with-slots (failures errors) result
    (unwind-protect-if handle-errors
	(handler-case-if handle-errors
	 (let ((res (progn (setup test)
			   (funcall (test-fn test) test))))
	   (when (typep res 'test-failure-condition)
	     (push (make-test-failure test res) failures)))
	 (test-failure-condition (failure)
	   (push (make-test-failure test failure) failures))
	 (error (err)
	   (push (make-test-failure test err) errors)))
	
	(if handle-errors
	    (handler-case
		(teardown test)
	      (error (err)
		(push (make-test-failure test err) errors)))
	    (teardown test))))
  result)


(defun make-test (fixture name &key test-fn test-suite description)
  "Create a test-case which is an instance of FIXTURE.  TEST-FN is
the method that will be invoked when perfoming this test, and can be a
symbol or a lambda taking a single argument, the test-fixture
instance.  DESCRIPTION is obviously what it says it is."
  (let ((newtest (make-instance fixture
		   :test-name (etypecase name
				(symbol
				 (string-downcase (symbol-name name)))
				(string
				 name))
		   :test-fn 
		   (if(and (symbolp name) (null test-fn))
		       name
		     test-fn)
		   :description description)))
    (when test-suite (add-test newtest test-suite))
    newtest))
