;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:       $Id: assert.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose:  Assert functions for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)


(define-condition assertion-failed (simple-condition) 
  ((message :initform nil :initarg :message :accessor message))
  (:documentation "Base class for all test failures."))

(defmethod print-object ((obj assertion-failed) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (apply #'format stream (simple-condition-format-control obj)
	   (simple-condition-format-arguments obj))))

(defun failure-message (message &optional format-str &rest args)
  "Signal a test failure and exit the test."
  (signal 'assertion-failed :message message :format-control format-str
	  :format-arguments args))

(defun failure (format-str &rest args)
  "Signal a test failure and exit the test."
  (apply #'failure-message nil format-str args))

(defun assert-equal (v1 v2 &optional message)
  (unless (equal v1 v2)
    (failure-message message "Assert equal: ~S ~S" v1 v2)))

(defun assert-eql (v1 v2 &optional message)
  (unless (eql v1 v2)
    (failure-message message "Assert equal: ~S ~S" v1 v2)))

(defun assert-not-eql (v1 v2 &optional message)
  (when (eql v1 v2)
    (failure-message message "Assert not eql: ~S ~S" v1 v2)))

(defmacro assert-true (v &optional message)
  `(unless ,v
    (failure-message ,message "Assert true: ~S" ',v)))

(defmacro assert-false (v &optional message)
  `(when ,v
     (failure-message ,message "Assert false: ~S" ',v)))

(defmacro assert-condition (condition form &optional message)
  (let ((cond (gensym "COND-")))
    `(handler-case
	 (progn
	   ,form
	   (values))
       (t (,cond)
	 (when (and (typep ,cond 'serious-condition)
		    (not (typep ,cond ,condition)))
	   (failure-message 
	    ,message 
	    "Assert condition ~A, but signaled condition ~A"
	    ,condition ,cond)))
       (:no-error ()
	 (failure-message ,message
			  "Assert condition ~A, but no condition signaled"
			  ,condition)))))

(defmacro assert-not-condition (condition form &optional message)
  (let ((cond (gensym "COND-")))
    `(handler-case
	 (progn
	   ,form
	   (values))
       (serious-condition (,cond)
	 (unless (typep ,cond ,condition)
	   (failure-message ,message "Assert not condition ~A"
			    ,condition))))))
