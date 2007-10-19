;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: result.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose:  Result functions for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)


(defclass test-results ()
  ((test :initarg :test :reader result-test)
   (count :initform 0 :accessor run-tests)
   (failures :initarg :failures :accessor failures :initform nil)
   (errors :initarg :errors :accessor errors :initform nil)
   (listeners :initform nil :accessor listeners)
   (stop :initform nil :accessor stop))
  (:documentation "Results of running test(s)"))

(defmethod failure-count ((res test-results))
  (length (failures res)))

(defmethod error-count ((res test-results))
  (length (errors res)))

(defun make-test-results ()
  (make-instance 'test-results))


(defmethod start-test ((tcase test) (res test-results))
  (incf (run-tests res))
  (mapc (lambda (listener)
	  (start-test listener tcase)) 
	(listeners res))
  res)

(defmethod end-test ((tcase test) (res test-results))
  (mapc (lambda (listener) (end-test listener tcase)) (listeners res))
  res)

(defmethod add-listener ((res test-results) (listener test-listener))
  (push listener (listeners res)))


;; Test Failures

(defclass test-failure ()
  ((failed-test :initarg :failed-test :reader failed-test)
   (thrown-condition :initarg :thrown-condition
		     :reader thrown-condition))
  (:documentation "Stored failures/errors in test-results slots"))

(defun make-test-failure (test condition)
  (make-instance 'test-failure :failed-test test
		 :thrown-condition condition))

(defmethod is-failure ((failure test-failure))
  "Returns T if a failure was a test-failure condition"
  (typep (thrown-condition failure) 'assertion-failed))

(defmethod print-object ((obj test-failure) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A: " (failed-test obj))
    (apply #'format stream 
	   (simple-condition-format-control (thrown-condition obj))
	   (simple-condition-format-arguments (thrown-condition obj)))))

(defmethod was-successful ((result test-results))
  "Returns T if a result has no failures or errors"
  (and (null (failures result)) (null (errors result))))


;----------------------------------------------------------------------
; methods  add-error, add-failure
;----------------------------------------------------------------------

(defmethod add-error ((ob test-results) (tcase test-case) condition)
    (push (make-test-failure tcase condition) (errors ob))
    (mapc #'(lambda (single-listener)
	      (add-error single-listener tcase condition))
	  (listeners ob)))


(defmethod add-failure ((ob test-results) (tcase test-case) condition)
  (push (make-test-failure tcase condition) (failures ob))
  (mapc #'(lambda (single-listener)
	    (add-failure single-listener tcase condition))
	(listeners ob)))

