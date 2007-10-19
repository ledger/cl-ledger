;;; -*- mode: lisp -*-
;;;
;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; Copyright (C) 2007  Pixel // pinterface
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

(defpackage :cl+ssl-system
  (:use :cl :asdf))

(in-package :cl+ssl-system)

(defsystem :cl+ssl
  :depends-on (:cffi :trivial-gray-streams :flexi-streams)
  :serial t
  :components
   ((:file "package")
    (:file "reload")
    (:file "conditions")
    (:file "ffi")
    (:file "ffi-buffer-all")
    #-clisp (:file "ffi-buffer")
    #+clisp (:file "ffi-buffer-clisp")
    (:file "streams")
    (:file "bio")))
