;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: suite.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Suite functions for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)

(defclass test-suite (test)
  ((name :initform "" :initarg :name :reader test-suite-name)
   (tests :initarg :tests :accessor tests :initform nil)
   (description :initarg :description :reader description
		:initform "No description.")))

(defmacro get-suite (class-name)
  `(suite (make-instance ',class-name)))
 

(defmethod add-test ((ob test-suite) (new-test test))
  (remove-test new-test ob)
  (setf (tests ob) (append (tests ob) (list new-test))))


(defmethod run-on-test-results ((ob test-suite) (result test-results)
				&key (handle-errors t))
  (mapc #'(lambda (composite)  ;;test-case or suite
            (run-on-test-results composite result
				:handle-errors handle-errors))
        (tests ob)))

(defmethod named-test (name (suite test-suite))
  (some (lambda (test-or-suite)
	  (when (and (typep test-or-suite 'test-case)
		     (equal name (name test-or-suite)))
	    test-or-suite))
	(tests suite)))

(defmethod remove-test ((test test) (suite test-suite))
  (setf (tests suite)
    (delete-if #'(lambda (existing-tests-or-suite)
		   (cond ((typep existing-tests-or-suite 'test-suite)
			  (eq existing-tests-or-suite test))
			 ((typep existing-tests-or-suite 'test-case)
			  (eql (name existing-tests-or-suite)
			       (name test)))))
	       (tests suite))))

;; Dynamic test suite

(defun find-test-generic-functions (instance)
  "Return a list of symbols for generic functions specialized on the
class of an instance and whose name begins with the string 'test-'.
This is used to dynamically generate a list of tests for a fixture."
  (let ((res)
	(package (symbol-package (class-name (class-of instance)))))
    (do-symbols (s package)
      (when (and (> (length (symbol-name s)) 5)
		 (string-equal "test-" (subseq (symbol-name s) 0 5))
		 (fboundp s)
		 (typep (symbol-function s) 'generic-function)
		 (ignore-errors
		   (plusp (length (compute-applicable-methods 
				   (ensure-generic-function s)
				   (list instance))))))
	(push s res)))
    (nreverse res)))


(defmacro def-test-method (method-name ((instance-name class-name)
					&key (run t))
			   &body method-body)
  `(let ((,instance-name
          (make-instance ',class-name
            :name ',method-name)))
     (setf (method-body ,instance-name)
           #'(lambda() ,@method-body))
     (add-test (suite ,instance-name) ,instance-name)
     (when ,run 
       (textui-test-run ,instance-name))))
