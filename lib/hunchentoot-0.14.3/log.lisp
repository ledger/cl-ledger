;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/log.lisp,v 1.8 2007/01/01 23:50:30 edi Exp $

;;; Copyright (c) 2004-2007, Dr. Edmund Weitz.  All rights reserved.

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

(defgeneric log-message (log-level fmt &rest args))

(defmethod log-message (log-level fmt &rest args)
  "Sends a formatted message to Apache's error log when the data gets
sent to Apache/mod_lisp and SERVER-USE-APACHE-LOG-P is true, otherwise
logs to the file denoted by LOG-FILE.  FMT and ARGS are as in FORMAT.
LOG-LEVEL is a keyword denoting the corresponding Apache error level."
  (let ((message (apply #'format nil fmt args)))
    (cond ((and (boundp '*server*)
                (server-mod-lisp-p *server*)
                (server-use-apache-log-p *server*))
           (with-input-from-string (s message)
             (loop with prolog = (case *log-prefix*
                                   ((nil) "")
                                   ((t) "[Hunchentoot] ")
                                   (otherwise (format nil "[~A] " *log-prefix*)))
                   for line = (read-line s nil nil)
                   while line
                   do (push (cons log-level
                                  (format nil "~A~A" prolog line))
                            (slot-value *reply* 'log-messages)))))
          (t (with-lock (*log-file-lock*)
               (ignore-errors
                 (unless *log-file-stream*
                   (let ((log-file-stream
                          (open (ensure-directories-exist *log-file*)
                                :direction :output
                                :element-type 'octet
                                :if-does-not-exist :create
                                :if-exists :append)))
                     (setq *log-file-stream*
                           (make-flexi-stream log-file-stream
                                              :external-format +utf-8+))))
                 (handler-case
                     (format *log-file-stream*
                             "[~A~@[ [~A]~]] ~A~%" (iso-time) log-level message)
                   (error ()
                     (format *log-file-stream* "[~A [EMERG]] A message could not be logged!"
                             (iso-time))))
                 (force-output *log-file-stream*))))))
  (values))

(defun log-message* (fmt &rest args)
  "Same as LOG-MESSAGE* but with the default log level \(as
defined by *DEFAULT-LOG-LEVEL*)."
  (apply #'log-message *default-log-level* fmt args))

(defun log-file ()
  "Returns the log file which is currently used."
  *log-file*)

(defun (setf log-file) (pathspec)
  "Sets the log file which is to be used."
  (with-lock (*log-file-lock*)
    (when *log-file-stream*
      (ignore-errors
        (close *log-file-stream*))
      (setq *log-file-stream* nil))
    (setq *log-file* pathspec)))
  