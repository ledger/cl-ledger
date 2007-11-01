;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/port-sbcl.lisp,v 1.12 2007/09/13 08:35:15 edi Exp $

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

#-:sb-unicode
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library needs a version of SBCL with Unicode support."))

#-:sb-thread
(eval-when (:compile-toplevel :load-toplevel :execute)
  (warn "Without thread support, this library is only useful for development."))

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun make-lock (name)
  "See SBCL documentation for SB-THREAD:MAKE-MUTEX."
  (sb-thread:make-mutex :name name))

(defmacro with-lock ((lock) &body body)
  "See SBCL documentation for SB-THREAD:WITH-RECURSIVE-LOCK."
  `(sb-thread:with-recursive-lock (,lock) ,@body))

(defvar *incf-mutex* (sb-thread:make-mutex :name "incf-mutex")
  "The mutex used for ATOMIC-INCF.")

(defmacro atomic-incf (place &optional (delta 1))
  "Like INCF but protected by a mutex, so other threads can't
interfer."
  `(with-lock (*incf-mutex*) (incf ,place ,delta)))

;; determine whether SB-EXT:WITH-TIMEOUT is supported; we can't just
;; use (FIND-SYMBOL "WITH-TIMEOUT" "SB-EXT") because sometimes (for
;; example in SBCL 1.0.6 for Win32) the function is present, but
;; doesn't work
(eval-when (:compile-toplevel :load-toplevel :execute)     
  (defun ensured-sleep-millis (milliseconds)
    "Sleeps \(in fact loops) not less then MILLISECONDS number of
milliseconds; the minimal sleep time is one internal time unit. Don't
use this function for large time values, because it eats processor
power."
    (do ((start-time (get-internal-real-time)))
        ((< (+ start-time (ceiling (* internal-time-units-per-second 
                                      (/ milliseconds 1000))))
            (get-internal-real-time)))))
  (cl:handler-case
      (sb-ext:with-timeout 0.0000001 (ensured-sleep-millis 5))
    (sb-ext:timeout ()
      (pushnew :hunchentoot-sbcl-with-timeout *features*))
    (t ())))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Executes the code BODY and returns the results of the last
form but stops execution after SECONDS seconds and then instead
executes the code in TIMEOUT-FORMS."
  (declare (ignorable seconds timeout-forms body))
  #-:hunchentoot-sbcl-with-timeout `(cl:progn ,@body)
  #+:hunchentoot-sbcl-with-timeout
  `(cl:handler-case
     (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout () ,@timeout-forms)))

(defun process-run-function (name function &rest args)
  "See SBCL documentation for SB-THREAD:MAKE-THREAD."
  (declare (ignorable name))
  #+:sb-thread
  (sb-thread:make-thread (lambda ()
                           (apply function args))
                         :name name)
  #-:sb-thread
  (apply function args))

(defun process-kill (process)
  "See SBCL documentation for SB-THREAD:TERMINATE-THREAD."
  (sb-thread:terminate-thread process))

(define-symbol-macro *current-process*
  sb-thread:*current-thread*)

(defun process-allow-scheduling ()
  "Used to simulate a function like PROCESS-ALLOW-SCHEDULING
which can be found in most other Lisps."
  (sleep .1))

(defun resolve-hostname (name)
  "Converts from different types to represent an IP address to
the canonical representation which is an array with four
integers."
  (typecase name
    (null #(0 0 0 0))
    (string (car (sb-bsd-sockets:host-ent-addresses
                  (sb-bsd-sockets:get-host-by-name name))))
    (integer (make-array 4 :initial-contents (list (ash name -24)
                                                   (logand (ash name -16) #xFF)
                                                   (logand (ash name -8) #xFF)
                                                   (logand name #xFF))))
    (t name)))

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
                       (setf socket (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream
                                       :protocol :tcp)
                             (sb-bsd-sockets:sockopt-reuse-address socket) t)
                       (sb-bsd-sockets:socket-bind socket (resolve-hostname address) service)
                       (sb-bsd-sockets:socket-listen socket 5)
                       (funcall announce socket)
                       (setq done socket)
                       (loop (funcall function (sb-bsd-sockets:socket-accept socket))))
                   (when socket
                     (cl:ignore-errors
                       (sb-bsd-sockets:socket-close socket))))))))
      (let ((listener-thread (process-run-function process-name #'open-socket-and-accept)))
        (loop until done do (sleep .1))
        (typecase done
          (sb-bsd-sockets:inet-socket listener-thread)
          (t (values nil done)))))))

(defun format-address (address)
  "Converts an array of four integers denoting an IP address into
the corresponding string representation."
  (format nil "~{~A~^.~}" (coerce address 'list)))

(defun make-socket-stream (socket read-timeout write-timeout)
  "Accepts a socket `handle' SOCKET and creates and returns a
corresponding stream, setting its read and write timeout if
applicable.  Returns three other values - the address the request
arrived at, and the address and port of the remote host."
  (declare (ignore write-timeout))
  (let ((local-host (sb-bsd-sockets:socket-name socket)))
    (multiple-value-bind (remote-host remote-port)
        (sb-bsd-sockets:socket-peername socket)
      (values (sb-bsd-sockets:socket-make-stream socket
                                                 :input t
                                                 :output t
                                                 :element-type '(unsigned-byte 8)
                                                 :timeout read-timeout
                                                 :buffering :full)
              (format-address local-host)
              (format-address remote-host)
              remote-port))))

;; determine how we're going to access the backtrace in the next
;; function
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "*DEBUG-PRINT-VARIABLE-ALIST*" :sb-debug)
    (pushnew :hunchentoot-sbcl-debug-print-variable-alist *features*)))

(defun get-backtrace (error)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace."
  (declare (ignore error))
  (with-output-to-string (s)
    #+:hunchentoot-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-variable-alist*
            (list* '(*print-level* . nil)
                   '(*print-length* . nil)
                   sb-debug:*debug-print-variable-alist*)))
      (sb-debug:backtrace most-positive-fixnum s))
    #-:hunchentoot-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-level* nil)
          (sb-debug:*debug-print-length* nil))
      (sb-debug:backtrace most-positive-fixnum s))))

