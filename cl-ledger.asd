;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;;
;; - Neither the name of New Artisans LLC nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(cl:in-package :asdf-user)

(defsystem :cl-ledger
  :serial t
  :description "Double-entry accounting system."
  :author "Johh Wiegley <jwiegley@gmail.com>"
  :maintainer "Christophe Junke <junke.christophe@gmail.com>"
  :license "BSD-3"
  :version "4.0.0"
  :depends-on (:local-time :periods-series :cambl :cl-ppcre)
  :components
  ((:module "core"
	    :components ((:file "packages")
			 (:file "types")
			 (:file "general")
			 (:file "ledger")
			 (:file "transaction")
			 (:file "normalize")
			 (:file "valexpr")
			 (:file "emacs"))
	    :serial t)

   (:module "transforms"
	    :components ((:file "totals")
			 (:file "filter")
			 (:file "periodic")
			 (:file "sort")
			 (:file "collapse")
			 (:file "invert")
			 (:file "subtotal")
			 (:file "related")
			 (:file "transform"))
	    :serial t)

   (:module "reports"
	    :components ((:file "report")
			 (:file "register")
			 (:file "sexp")
			 (:file "balance")
			 (:file "print")
			 (:file "entry"))
	    :serial t)

   (:module "parsers"
	    :components
	    ((:module "textual"
	      :components ((:file "textual")
			   (:file "autoentry" :depends-on ("textual"))
			   (:file "perentry" :depends-on ("textual")))
	      :serial t)))

   (:file "driver")))
