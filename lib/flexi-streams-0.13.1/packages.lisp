;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/packages.lisp,v 1.29 2007/10/11 06:56:49 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz.  All rights reserved.

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

(unless (find-symbol (if (eq (readtable-case *readtable*) :upcase)
                       "STREAM-FILE-POSITION"
                       "stream-file-position")
                     :trivial-gray-streams)
  (error "You need a newer version of TRIVIAL-GRAY-STREAMS."))

(defpackage :flexi-streams
  (:use :cl :trivial-gray-streams)
  (:nicknames :flex)
  #+:lispworks
  (:shadow :with-accessors)
  (:export :*default-eol-style*
           :*default-little-endian*
           :*substitution-char*
           :external-format-eol-style
           :external-format-equal
           :external-format-id
           :external-format-little-endian
           :external-format-name
           :flexi-input-stream
           :flexi-output-stream
           :flexi-io-stream
           :flexi-stream
           :flexi-stream-bound
           :flexi-stream-external-format
           :flexi-stream-encoding-error
           :flexi-stream-element-type
           :flexi-stream-element-type-error
           :flexi-stream-element-type-error-element-type
           :flexi-stream-error
           :flexi-stream-column
           :flexi-stream-position
           :flexi-stream-position-spec-error
           :flexi-stream-position-spec-error-position-spec
           :flexi-stream-stream
           :get-output-stream-sequence
           :in-memory-stream
           :in-memory-stream-closed-error
           :in-memory-stream-error
           :in-memory-input-stream
           :in-memory-output-stream
           :list-stream
           :make-external-format
           :make-in-memory-input-stream
           :make-in-memory-output-stream
           :make-flexi-stream
           :octet
           :octets-to-string
           :output-stream-sequence-length
           :peek-byte
           :string-to-octets
           :unread-byte
           :vector-stream
           :with-input-from-sequence
           :with-output-to-sequence))
