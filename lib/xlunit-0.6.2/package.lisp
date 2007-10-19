;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: package.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Package definition for XLUnit
;;;;
;;;; $Id: package.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:xlunit
  (:use #:cl)
  (:export
   
   ;; test-case.lisp
   #:test-case
   #:def-test-method
   #:set-up
   #:tear-down
   #:run
   #:run-test
   #:make-test

   ;; assert
   #:assert-equal
   #:assert-eql
   #:assert-not-eql
   #:assert-true
   #:assert-false
   #:assert-condition
   #:test
   #:test-error
   #:test-no-error
   #:test-warning
   #:test-no-warning
   #:failure

   ;; suite.lisp
   #:textui-test-run
   #:make-test-suite
   #:setup-testsuite-named
   #:teardown-testsuite-named
   #:add-test
   #:named-test
   #:remove-test
   #:tests
   #:get-suite
   #:suite
   #:test-suite
   #:run-on-test-results
   
   ;; printer.lisp
   #:summary
   
   ;; result.lisp
   #:test-results
   #:make-test-results
   #:was-successful
   )
  (:documentation "This is the XLUnit Framework."))

