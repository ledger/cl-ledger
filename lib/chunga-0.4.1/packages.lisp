;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/chunga/packages.lisp,v 1.14 2007/09/18 07:00:39 edi Exp $

;;; Copyright (c) 2006-2007, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :chunga
  (:use :cl :trivial-gray-streams :flexi-streams)
  #+:lispworks
  (:import-from :lw :when-let)
  (:export :*accept-bogus-eols*
           :*current-error-message*
           :*treat-semicolon-as-continuation*
           :assert-char
           :chunked-input-stream
           :chunked-input-stream-extensions
           :chunked-input-stream-trailers
           :chunked-io-stream
           :chunked-output-stream
           :chunked-stream
           :chunked-stream-input-chunking-p
           :chunked-stream-output-chunking-p
           :chunked-stream-stream
           :input-chunking-body-corrupted
           :input-chunking-unexpected-end-of-file
           :make-chunked-stream
           :read-http-headers
           :read-line*
           :read-name-value-pair
           :read-name-value-pairs
           :read-token
           :skip-whitespace
           :trim-whitespace))
           
