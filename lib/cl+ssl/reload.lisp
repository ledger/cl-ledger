;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

;;; We do this in an extra file so that it happens
;;;   - after the asd file has been loaded, so that users can
;;;     customize *libssl-pathname* between loading the asd and LOAD-OPing
;;;     the actual sources
;;;   - before ssl.lisp is loaded, which needs the library at compilation
;;;     time on some implemenations
;;;   - but not every time ffi.lisp is re-loaded as would happen if we
;;;     put this directly into ffi.lisp

(in-package :cl+ssl)

(cffi:define-foreign-library libssl
  (:windows "libssl32.dll")
  ;(:unix (:or "libssl.so.0.9.8" "libssl.so"))
  (:unix (:or "libssl.dylib" "libssl.so"))
  (t (:default "libssl3")))

(cffi:use-foreign-library libssl)

(cffi:define-foreign-library libeay32
  (:windows "libeay32.dll"))

(cffi:use-foreign-library libeay32)
