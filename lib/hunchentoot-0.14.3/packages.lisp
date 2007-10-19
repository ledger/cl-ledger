;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/packages.lisp,v 1.33 2007/09/18 14:23:23 edi Exp $

;;; Copyright (c) 2004-2007, Dr. Edmund Weitz. All rights reserved.

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

(defpackage :hunchentoot-mp
  (:nicknames :tbnl-mp)
  (:use :cl)
  (:export :*current-process*
           :make-lock
           :with-lock
           :process-run-function
           :process-kill))

(defpackage :hunchentoot
  (:nicknames :tbnl)
  (:use :cl :cl-ppcre :chunga :flexi-streams :url-rewrite :hunchentoot-mp)
  (:shadow :assoc
           #+:sbcl :defconstant
           :handler-case
           :ignore-errors
           :url-encode)
  ;; see ASDF system definition
  (:import-from :hunchentoot-asd :*hunchentoot-version*)
  #+:lispworks
  (:import-from :lw :with-unique-names :when-let)
  (:export :*approved-return-codes*
           :*catch-errors-p*
           :*cleanup-function*
           :*cleanup-interval*
           :*content-types-for-url-rewrite*
           :*default-content-type*
           :*default-handler*
           :*default-log-level*
           :*default-read-timeout*
           :*default-write-timeout*
           :*dispatch-table*
           :*file-upload-hook*
           :*handle-http-errors-p*
           :*header-stream*
           :*http-error-handler*
           :*hunchentoot-default-external-format*
           :*lisp-errors-log-level*
           :*lisp-warnings-log-level*
           :*listener*
           :*log-lisp-backtraces-p*
           :*log-lisp-errors-p*
           :*log-lisp-warnings-p*
           :*log-prefix*
           :*meta-dispatcher*
           :*methods-for-post-parameters*
           :*reply*
           :*request*
           :*rewrite-for-session-urls*
           :*server*
           :*session*
           :*session-cookie-name*
           :*session-gc-frequency*
           :*session-max-time*
           :*session-removal-hook*
           :*show-access-log-messages*
           :*show-lisp-backtraces-p*
           :*show-lisp-errors-p*
           :*tmp-directory*
           :*use-remote-addr-for-sessions*
           :*use-user-agent-for-sessions*
           :+http-accepted+
           :+http-authorization-required+
           :+http-bad-gateway+
           :+http-bad-request+
           :+http-conflict+
           :+http-continue+
           :+http-created+
           :+http-expectation-failed+
           :+http-failed-dependency+
           :+http-forbidden+
           :+http-gateway-time-out+
           :+http-gone+
           :+http-internal-server-error+
           :+http-length-required+
           :+http-method-not-allowed+
           :+http-moved-permanently+
           :+http-moved-temporarily+
           :+http-multiple-choices+
           :+http-multi-status+
           :+http-no-content+
           :+http-non-authoritative-information+
           :+http-not-acceptable+
           :+http-not-found+
           :+http-not-implemented+
           :+http-not-modified+
           :+http-ok+
           :+http-partial-content+
           :+http-payment-required+
           :+http-precondition-failed+
           :+http-proxy-authentication-required+
           :+http-request-entity-too-large+
           :+http-request-time-out+
           :+http-request-uri-too-large+
           :+http-requested-range-not-satisfiable+
           :+http-reset-content+
           :+http-see-other+
           :+http-service-unavailable+
           :+http-switching-protocols+
           :+http-temporary-redirect+
           :+http-unsupported-media-type+
           :+http-use-proxy+
           :+http-version-not-supported+
           :authorization
           :aux-request-value
           :content-length
           :content-type
           :cookie-domain
           :cookie-expires
           :cookie-http-only
           :cookie-in
           :cookie-name
           :cookie-out
           :cookie-path
           :cookie-secure
           :cookie-value
           :cookies-in
           :cookies-out
           :create-folder-dispatcher-and-handler
           :create-prefix-dispatcher
           :create-regex-dispatcher
           :create-static-file-dispatcher-and-handler
           :default-dispatcher
           :define-easy-handler
           :delete-aux-request-value
           :delete-session-value
           :dispatch-easy-handlers
           :dispatch-request
           :do-sessions
           :escape-for-html
           :get-backtrace
           :get-parameter
           :get-parameters
           :handle-if-modified-since
           :handle-static-file
           :handler-done
           :header-in
           :header-out
           :headers-in
           :headers-out
           :host
           :http-token-p
           :log-file
           :log-message
           :log-message*
           :maybe-invoke-debugger
           :mime-type
           :mod-lisp-id
           :no-cache
           :parameter
           :post-parameter
           :post-parameters
           :query-string
           :raw-post-data
           :real-remote-addr
           :reason-phrase
           :recompute-request-parameters
           :redirect
           :referer
           :remote-addr
           :remote-port
           :remove-session
           :reply-external-format
           :request-method
           :request-uri
           :require-authorization
           :reset-sessions
           :return-code
           :rfc-1123-date
           :script-name
           :send-headers
           :server-addr
           :server-address
           :server-dispatch-table
           :server-local-port
           :server-name
           :server-port
           :server-protocol
           :session-counter
           :session-gc
           :session-max-time
           :session-too-old-p
           :session-remote-addr
           :session-cookie-value
           :session-user-agent
           :session-value
           :set-cookie
           :set-cookie*
           :ssl-p
           :ssl-session-id
           :start-server
           :start-session
           :stop-server
           :url-decode
           :url-encode
           :user-agent))

