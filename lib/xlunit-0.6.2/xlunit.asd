;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          xlunit.asd
;;;; Purpose:       ASDF definition file for Xlunit
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: xlunit.asd 7061 2003-09-07 06:34:45Z kevin $
;;;; *************************************************************************

(defpackage #:xlunit-system (:use #:asdf #:cl))
(in-package #:xlunit-system)

(defsystem xlunit
  :name "xlunit"
  :author "Kevin Rosenberg based on work by Craig Brozensky"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "BSD"
  :description "Extreme Lisp Testing Suite"
  :long-description "The XLUnit package is toolkit for building test suites. It is based on the XPTest package by Craig Brozensky and the JUnit package by Kent Beck."

  :properties ((#:author-email . "kevin@rosenberg.net")
	       ((#:albert #:output-dir) . "albert-docs/")
	       ((#:albert #:formats) . ("docbook"))
	       ((#:albert #:docbook #:template) . "book")
	       ((#:albert #:docbook #:bgcolor) . "white")
	       ((#:albert #:docbook #:textcolor) . "black"))
  
  :serial t
  :components
  ((:file "package")
   (:file "assert")
   (:file "tcase")
   (:file "listener")
   (:file "result")
   (:file "suite")
   (:file "textui")
   (:file "printer")
   ))

(defmethod perform ((o test-op) (c (eql (find-system 'xlunit))))
  (operate 'load-op 'xlunit-tests)
  (operate 'test-op 'xlunit-tests :force t))

(defsystem xlunit-tests
    :depends-on (xlunit)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'xlunit-tests))))
  (operate 'load-op 'xlunit-tests)
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:xlunit-tests)))
      (error "test-op failed")))

