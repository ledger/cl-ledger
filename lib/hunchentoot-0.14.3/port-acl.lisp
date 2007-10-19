;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/port-acl.lisp,v 1.9 2007/04/19 09:10:39 edi Exp $

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(and :allegro-version>= (version>= 7 0))
  (error "You need at least version 7.0 of AllegroCL.")
  ;; make sure code for sockets and OS interface is loaded
  (require :sock)
  (require :osi))

(defun make-lock (name)
  "See AllegroCL documentation for MP:MAKE-PROCESS-LOCK."
  (mp:make-process-lock :name name))

(defmacro with-lock ((lock) &body body)
  "See AllegroCL documentation for MP:WITH-PROCESS-LOCK."
  `(mp:with-process-lock (,lock) ,@body))

(defmacro atomic-incf (place &optional (delta 1))
  "Like INCF but wrapped with SYS:WITHOUT-SCHEDULING so other
threads can't interfer."
  `(sys:without-scheduling (incf ,place ,delta)))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "See AllegroCL documentation for SYS:WITH-TIMEOUT."
  `(sys:with-timeout (,seconds ,@timeout-forms) ,@body))

(defun process-run-function (name function &rest args)
  "See AllegroCL documentation for MP:PROCESS-RUN-FUNCTION."
  (apply #'mp:process-run-function name function args))

(defun process-kill (process)
  "See AllegroCL documentation for MP:PROCESS-KILL."
  (mp:process-kill process))

(define-symbol-macro *current-process*
  mp:*current-process*)

(defun process-allow-scheduling ()
  "See AllegroCL documentation for MP:PROCESS-ALLOW-SCHEDULE."
  (mp:process-allow-schedule))

(defun start-up-server (&key service address process-name announce function &allow-other-keys)
  "Tries to \(partly) emulate LispWorks' COMM:START-UP-SERVER.  See
<http://www.lispworks.com/documentation/lw50/LWRM/html/lwref-56.htm>
for more info."
  (let (done)
    (flet ((open-socket-and-accept ()
             (handler-bind ((error (lambda (condition)
                                     (funcall announce nil condition)
                                     (setq done condition)
                                     (return-from open-socket-and-accept))))
               (let (socket)
                 (unwind-protect
                     (progn
                       (setf socket (socket:make-socket :address-family :internet
                                       :type :hiper
                                       :format :bivalent
                                       :connect :passive
                                       :local-host address
                                       :local-port service
                                       :reuse-address t
                                       :backlog 5))
                       (funcall announce socket)
                       (setq done socket)
                       (loop (funcall function (socket:accept-connection socket :wait t))))
                   (when socket
                     (cl:ignore-errors (close socket))))))))
      (let ((listener-thread (process-run-function process-name #'open-socket-and-accept)))
        (mp:process-wait "Waiting for server to start" (lambda () done))
        (typecase done
          (socket:socket listener-thread)
          (t (values nil done)))))))

(defun make-socket-stream (socket read-timeout write-timeout)
  "Accepts a socket `handle' SOCKET and creates and returns a
corresponding stream, setting its read and write timeout if
applicable.  Returns three other values - the address the request
arrived at, and the address and port of the remote host."
  ;; in the case of AllegroCL, SOCKET:ACCEPT-CONNECTION already
  ;; returned a stream
  (socket:set-socket-options socket :nodelay t)
  (socket:socket-control socket
                         :read-timeout read-timeout
                         :write-timeout write-timeout)
  (values socket
          (socket:ipaddr-to-dotted (socket:local-host socket))
          (socket:ipaddr-to-dotted (socket:remote-host socket))
          (socket:remote-port socket)))

(defun get-backtrace (error)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace."
  (with-output-to-string (s)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (*print-miser-width* 40)
            (*print-pretty* t)
            (tpl:*zoom-print-circle* t)
            (tpl:*zoom-print-level* nil)
            (tpl:*zoom-print-length* nil))
        (cl:ignore-errors
          (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
                  error))
        (cl:ignore-errors
          (let ((*terminal-io* s)
                (*standard-output* s))
            (tpl:do-command "zoom"
                            :from-read-eval-print-loop nil
                            :count t
                            :all t)))))))

