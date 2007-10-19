;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl+ssl
  (:use :common-lisp :trivial-gray-streams)
  (:export #:ensure-initialized
           #:reload
	   #:stream-fd
	   #:make-ssl-client-stream
           #:make-ssl-server-stream))
