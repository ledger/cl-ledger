;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/flexi-streams.asd,v 1.57 2007/10/11 06:56:49 edi Exp $

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

(defpackage :flexi-streams-system
  (:use :asdf :cl))

(in-package :flexi-streams-system)

(defsystem :flexi-streams
  :version "0.13.1"
  :serial t
  :components ((:file "packages")
	       (:file "ascii")
	       (:file "koi8-r")
               (:file "iso-8859")
               (:file "code-pages")
               (:file "specials")
               (:file "util")
               (:file "external-format")
               (:file "in-memory")
               (:file "stream")
               #+:lispworks (:file "lw-binary-stream")
               (:file "output")
               (:file "input")
               (:file "strings"))
  :depends-on (:trivial-gray-streams))

(defsystem :flexi-streams-test
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test"))))
  :depends-on (:flexi-streams))

(defmethod perform ((o test-op) (c (eql (find-system 'flexi-streams))))
  (operate 'load-op 'flexi-streams-test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :flexi-streams-test))))
