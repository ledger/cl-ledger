;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/port-mcl.lisp,v 1.8 2007/01/01 23:50:30 edi Exp $

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

(defun make-lock (name)
  "See OpenMCL documentation for CCL:MAKE-LOCK."
  (ccl:make-lock name))

(defmacro with-lock ((lock) &body body)
  "See OpenMCL documentation for CCL:WITH-LOCK-GRABBED."
  `(ccl:with-lock-grabbed (,lock) ,@body))

(defmacro atomic-incf (place &optional (delta 1))
  "Like INCF, but other threads can't interfer."
  `(ccl::atomic-incf-decf ,place ,delta))

(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  "Executes the function \(with no arguments) BODY-FN and returns
its results but stops execution after DURATION seconds and then
instead calls TIMEOUT-FN and returns its values."
  ;; from Portable AllegroServe
  (block timeout
    (let* ((timer (ccl::make-timer-request seconds
                                           #'(lambda ()
                                               (return-from timeout (funcall timeoutfn))))))
      (ccl::enqueue-timer-request timer)
      (unwind-protect (funcall bodyfn)
	(ccl::dequeue-timer-request timer)))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Executes the code BODY and returns the results of the last
form but stops execution after SECONDS seconds and then instead
executes the code in TIMEOUT-FORMS."
  ;; from Portable AllegroServe
  `(invoke-with-timeout ,seconds
                        #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))

(defun process-run-function (name function &rest args)
  "See OpenMCL documentation for CCL:PROCESS-RUN-FUNCTION."
  (apply #'ccl:process-run-function name function args))

(defun process-kill (process)
  "See OpenMCL documentation for CCL:PROCESS-KILL."
  (ccl:process-kill process))

(define-symbol-macro *current-process*
  ccl:*current-process*)

(defun process-allow-scheduling ()
  "See OpenMCL documentation for CCL:PROCESS-ALLOW-SCHEDULE"
  (ccl:process-allow-schedule))

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
                       (setf socket (ccl:make-socket :address-family :internet
                                                     :type :stream
                                                     :connect :passive
                                                     :local-host address
                                                     :local-port service
                                                     :reuse-address t
                                                     :backlog 5))
                       (funcall announce socket)
                       (setq done socket)
                       (loop (funcall function (ccl:accept-connection socket :wait t))))
                   (when socket
                     (cl:ignore-errors
                       (close socket))))))))
      (let ((listener-thread (process-run-function process-name #'open-socket-and-accept)))
        (ccl:process-wait "Waiting for server to start" (lambda () done))
        (typecase done
          (condition (values nil done))
          (t listener-thread))))))

(defun make-socket-stream (socket read-timeout write-timeout)
  "Accepts a socket `handle' SOCKET and creates and returns a
corresponding stream, setting its read and write timeout if
applicable.  Returns three other values - the address the request
arrived at, and the address and port of the remote host."
  (declare (ignore read-timeout write-timeout))
  (values socket
          (ccl:ipaddr-to-dotted (ccl:local-host socket))
          (ccl:ipaddr-to-dotted (ccl:remote-host socket))
          (ccl:remote-port socket)))

(defun get-backtrace (error)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace."
  (with-output-to-string (s)
    (let ((*debug-io* s))
      (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
              error)
      (ccl:print-call-history :detailed-p nil))))
