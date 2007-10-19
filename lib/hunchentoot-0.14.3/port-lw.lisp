;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/port-lw.lisp,v 1.9 2007/03/09 10:50:24 edi Exp $

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

(in-package :hunchentoot)

#+(and :lispworks4.4 (or :win32 :linux))
(let ((id :system-cons-free-chain))
  (unless (scm::patch-id-loaded-p id)
    (error "You need a patch to improve the performance of this code. Request patch ~S for ~A for ~A from lisp-support@lispworks.com using the Report Bug command."
          id (lisp-implementation-type)
          #+:win32 "Windows"
          #+:linux "Linux")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; make sure socket code is loaded
  (require "comm"))

(defun make-lock (name)
  "See LispWorks documentation for MP:MAKE-LOCK."
  (mp:make-lock :name name))

(defmacro with-lock ((lock) &body body)
  "See LispWorks documentation for MP:WITH-LOCK."
  `(mp:with-lock (,lock) ,@body))

(defmacro atomic-incf (place &optional (delta 1))
  "Like INCF but wrapped with MP:WITHOUT-PREEMPTION so other
threads can't interfer."
  `(mp:without-preemption (incf ,place ,delta)))

(defun invoke-with-timeout (duration body-fn timeout-fn)
  "Executes the function \(with no arguments) BODY-FN and returns
its results but stops execution after DURATION seconds and then
instead calls TIMEOUT-FN and returns its values."
  ;; from Portable AllegroServe
  (block timeout
    (let* ((process mp:*current-process*)
           (unsheduledp nil)
           (timer (mp:make-timer
                   #'(lambda ()
                       (mp:process-interrupt process
                                             #'(lambda ()
                                                 (unless unsheduledp
                                                   (return-from timeout
                                                     (funcall timeout-fn)))))))))
      (mp:schedule-timer-relative timer duration)
      (unwind-protect
          (funcall body-fn)
        (mp:without-interrupts
         (mp:unschedule-timer timer)
         (setf unsheduledp t))))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Executes the code BODY and returns the results of the last
form but stops execution after SECONDS seconds and then instead
executes the code in TIMEOUT-FORMS."
  ;; from Portable AllegroServe
  `(invoke-with-timeout ,seconds
                        #'(lambda ()
                            ,@body)
                        #'(lambda ()
                            ,@timeout-forms)))

(defun process-run-function (name function &rest args)
  "See LispWorks documentation for MP:PROCESS-RUN-FUNCTION."
  (apply #'mp:process-run-function name nil function args))

(defun process-kill (process)
  "See LispWorks documentation for MP:PROCESS-KILL."
  (mp:process-kill process))

(define-symbol-macro *current-process*
  mp:*current-process*)

(defun process-allow-scheduling ()
  "See LispWorks documentation for MP:PROCESS-ALLOW-SCHEDULING."
  (mp:process-allow-scheduling))

(defun start-up-server (&rest args)
  "See LispWorks documentation for COMM:START-UP-SERVER."
  (apply #'comm:start-up-server args))

(defun make-socket-stream (socket read-timeout write-timeout)
  "Accepts a socket `handle' SOCKET and creates and returns a
corresponding stream, setting its read and write timeout if
applicable.  Returns three other values - the address the request
arrived at, and the address and port of the remote host."
  #-:lispworks5.0 (declare (ignore write-timeout))
  (let ((local-host (comm:get-socket-address socket)))
    (multiple-value-bind (remote-host remote-port)
        (comm:get-socket-peer-address socket)
      (values (make-instance 'comm:socket-stream
                 :socket socket
                 :direction :io
                 :read-timeout read-timeout
                 #+:lispworks5.0 #+:lispworks5.0
                 :write-timeout write-timeout
                 :element-type '(unsigned-byte 8))
              (comm:ip-address-string local-host)
              (comm:ip-address-string remote-host)
              remote-port))))

#-:hunchentoot-no-ssl
(defun make-ssl-server-stream (socket-stream &key certificate-file privatekey-file privatekey-password)
  "Given the server socket stream SOCKET-STREAM attaches SSL to the
stream using the certificate file CERTIFICATE-FILE and the private key
file PRIVATEKEY-FILE.  Both of these values must be namestrings
denoting the location of the files.  If PRIVATEKEY-PASSWORD is not NIL
then it should be the password for the private key file \(if
necessary)."
  (flet ((ctx-configure-callback (ctx)
           (when privatekey-password
             (comm:set-ssl-ctx-password-callback ctx :password privatekey-password))
           (comm:ssl-ctx-use-certificate-file ctx
                                              certificate-file
                                              comm:ssl_filetype_pem)
           (comm:ssl-ctx-use-privatekey-file ctx
                                             privatekey-file
                                             comm:ssl_filetype_pem)))
    (comm:attach-ssl socket-stream
                     :ctx-configure-callback #'ctx-configure-callback)))

(defun get-backtrace (error)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace."
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((dbg::*debugger-stack* (dbg::grab-stack nil :how-many most-positive-fixnum))
          (*debug-io* s)
          (dbg:*debug-print-level* nil)
          (dbg:*debug-print-length* nil))
      (dbg:bug-backtrace nil))))

;; some help for the IDE
(dspec:define-dspec-alias defvar-unbound (name)
  `(defparameter ,name))

(dspec:define-dspec-alias def-http-return-code (name)
  `(defconstant ,name))

(editor:setup-indent "defvar-unbound" 1 2 4)

(editor:setup-indent "def-http-return-code" 1 2 4)

