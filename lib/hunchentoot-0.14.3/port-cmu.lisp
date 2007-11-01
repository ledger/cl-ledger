;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/port-cmu.lisp,v 1.9 2007/01/01 23:50:30 edi Exp $

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

#-:mp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library needs a version of CMUCL with MP support."))

(defun make-lock (name)
  "See CMUCL documentation for MP:MAKE-LOCK."
  (mp:make-lock name))

(defmacro with-lock ((lock) &body body)
  "See CMUCL documentation for MP:WITH-LOCK-HELD."
  `(mp:with-lock-held (,lock) ,@body))

(defmacro atomic-incf (place &optional (delta 1))
  "Like INCF but wrapped with MP:WITHOUT-SCHEDULING so other
threads can't interfer."
  `(mp:without-scheduling (incf ,place ,delta)))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "See CMUCL documentation for MP:WITH-TIMEOUT."
  `(mp:with-timeout (,seconds ,@timeout-forms) ,@body))

(defun process-run-function (name function &rest args)
  "See CMUCL documentation for MP:MAKE-PROCESS."
  (mp:make-process (lambda ()
                     (apply function args))
                   :name name))

(defun process-kill (process)
  "See CMUCL documentation for MP:DESTROY-PROCESS."
  (mp:destroy-process process))

(define-symbol-macro *current-process*
  mp:*current-process*)

(defun process-allow-scheduling ()
  "See CMUCL documentation for MP:PROCESS-YIELD."
  (mp:process-yield))

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
                       (setf socket (ext:create-inet-listener service :stream
                                                              :reuse-address t
                                                              :backlog 5
                                                              :host (or address 0)))
                       (funcall announce socket)
                       (setq done socket)
                       (loop (funcall function (ext:accept-tcp-connection socket))))
                   (when socket
                     (cl:ignore-errors
                       (ext:close-socket socket))))))))
      (let ((listener-thread (process-run-function process-name #'open-socket-and-accept)))
        (mp:process-wait "Waiting for server to start" (lambda () done))
        (typecase done
          (condition (values nil done))
          (t listener-thread))))))

(defun format-address (address)
  "Converts an integer in network byte order denoting an IP
address into the corresponding string representation."
  (format nil "~A.~A.~A.~A"
          (ash address -24)
          (logand (ash address -16) #xFF)
          (logand (ash address -8) #xFF)
          (logand address #xFF)))

(defun make-socket-stream (handle read-timeout write-timeout)
  "Accepts a socket `handle' HANDLE and creates and returns a
corresponding stream, setting its read and write timeout if
applicable.  Returns three other values - the address the request
arrived at, and the address and port of the remote host."
  (declare (ignore write-timeout))
  (let ((local-host (ext:get-socket-host-and-port handle)))
    (multiple-value-bind (remote-host remote-port)
        (ext:get-peer-host-and-port handle)
      (values (sys:make-fd-stream handle
                                  :input t :output t
                                  :element-type '(unsigned-byte 8)
                                  :auto-close t
                                  :buffering :full
                                  :timeout read-timeout
                                  :name (format nil "~A:~A" (format-address remote-host) remote-port))
              (format-address local-host)
              (format-address remote-host)
              remote-port))))

(defun get-backtrace (error)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace."
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((debug:*debug-print-level* nil)
          (debug:*debug-print-length* nil))
      (debug:backtrace most-positive-fixnum s))))

