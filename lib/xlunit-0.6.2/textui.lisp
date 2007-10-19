;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: textui.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Text UI for Test Runner
;;;;
;;;; *************************************************************************

(in-package #:xlunit)

;;; Test Runners

(defclass textui-test-runner (test-listener)
  ((ostream :initform nil :accessor ostream :initarg :ostream))
  (:default-initargs :ostream *standard-output*))
 
(defmethod add-error ((ob textui-test-runner) test-case condition)
  (declare (ignore test-case condition))
  (format (ostream ob) "E"))
   
(defmethod add-failure ((ob textui-test-runner) test-case condition)
  (declare (ignore test-case condition))
  (format (ostream ob) "F"))
   
(defmethod start-test ((ob textui-test-runner) test-case)
  (declare (ignore test-case))
  (format (ostream ob) "."))
 

(defmethod textui-test-run ((ob test))
  (let ((test-runner (make-instance 'textui-test-runner))
        (result (make-instance 'test-results))
	(start-time (get-internal-real-time)))
    (add-listener result test-runner)
    (run-on-test-results ob result)
    (print-results test-runner result 
		   (/ (- (get-internal-real-time) start-time)
		      internal-time-units-per-second))
    result))
