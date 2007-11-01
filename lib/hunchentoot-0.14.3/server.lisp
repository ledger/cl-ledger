;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/server.lisp,v 1.37 2007/09/06 23:32:10 edi Exp $

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

(defclass server ()
  ((socket :accessor server-socket
           :documentation "The socket the server is listening on.")
   (port :initarg :port
         :reader server-local-port
         :documentation "The port the server is listening on.
See START-SERVER.")
   (address :initarg :address
            :reader server-address
            :documentation "The address the server is listening
on.  See START-SERVER.")
   (name :initarg :name
         :accessor server-name
         :documentation "The optional name of the server, a symbol.")
   (dispatch-table :initarg :dispatch-table
                   :accessor server-dispatch-table
                   :documentation "The dispatch-table used by this
server.  Can be NIL to denote that *META-DISPATCHER* should be called
instead.")
   (output-chunking-p :initarg :output-chunking-p
                      :reader server-output-chunking-p
                      :documentation "Whether the server may use output chunking.")
   (input-chunking-p :initarg :input-chunking-p
                     :reader server-input-chunking-p
                     :documentation "Whether the server may use input chunking.")
   (read-timeout :initarg :read-timeout
                 :reader server-read-timeout
                 :documentation "The read-timeout of the server.")
   (write-timeout :initarg :write-timeout
                  :reader server-write-timeout
                  :documentation "The write-timeout of the server.")
   (listener :accessor server-listener
             :documentation "The Lisp process which listens for
incoming requests and starts new worker threads for each new
connection.")
   (workers :initform nil
            :accessor server-workers
            :documentation "A list of currently active worker threads.")
   (mod-lisp-p :initform nil
               :initarg :mod-lisp-p
               :reader server-mod-lisp-p
               :documentation "Whether this is a genuine
Hunchentoot server or \"just\" infrastructure for mod_lisp.")
   (use-apache-log-p :initarg :use-apache-log-p
                     :reader server-use-apache-log-p
                     :documentation "Whether the server should use
Apache's log file.  Only applicable if MOD-LISP-P is true.")
   #-:hunchentoot-no-ssl
   (ssl-certificate-file :initarg :ssl-certificate-file
                         :reader server-ssl-certificate-file
                         :documentation "The namestring of a
certificate file if SSL is used, NIL otherwise.")
   #-:hunchentoot-no-ssl
   (ssl-privatekey-file :initarg :ssl-privatekey-file
                        :reader server-ssl-privatekey-file
                        :documentation "The namestring of a
private key file if SSL is used, NIL otherwise.")
   #-:hunchentoot-no-ssl
   (ssl-privatekey-password :initarg :ssl-privatekey-password
                            :reader server-ssl-privatekey-password
                            :documentation "The password for the
private key file or NIL.")
   (lock :initform (make-lock (format nil "hunchentoot-lock-~A"
                                      *server-counter*))
         :reader server-lock
         :documentation "A lock which is used to make sure that
we can shutdown the server cleanly."))
  (:documentation "An object of this class contains all relevant
information about a running Hunchentoot server instance."))

(defun start-server (&key (port 80 port-provided-p)
                          address
                          dispatch-table
                          (name (gensym))
                          (mod-lisp-p nil)
                          (use-apache-log-p mod-lisp-p)
                          (input-chunking-p t)
                          (read-timeout *default-read-timeout*)
                          (write-timeout *default-write-timeout*)
                          #+(and :unix (not :win32)) setuid
                          #+(and :unix (not :win32)) setgid
                          #-:hunchentoot-no-ssl ssl-certificate-file
                          #-:hunchentoot-no-ssl (ssl-privatekey-file ssl-certificate-file)
                          #-:hunchentoot-no-ssl ssl-privatekey-password)
  "Starts a Hunchentoot server and returns the SERVER object \(which
can be stopped with STOP-SERVER).  PORT is the port the server will be
listening on - the default is 80 \(or 443 if SSL information is
provided).  If ADDRESS is a string denoting an IP address, then the
server only receives connections for that address.  This must be one
of the addresses associated with the machine and allowed values are
host names such as \"www.nowhere.com\" and address strings like
\"204.71.177.75\".  If ADDRESS is NIL, then the server will receive
connections to all IP addresses on the machine.  This is the default.

DISPATCH-TABLE can either be a dispatch table which is to be used by
this server or NIL which means that at request time *META-DISPATCHER*
will be called to retrieve a dispatch table.

NAME should be a symbol which can be used to name the server.  This
name can utilized when defining \"easy handlers\" - see
DEFINE-EASY-HANDLER.  The default name is an uninterned symbol as
returned by GENSYM.

If MOD-LISP-P is true, the server will act as a back-end for mod_lisp,
otherwise it will be a stand-alone web server.  If USE-APACHE-LOG-P is
true, log messages will be written to the Apache log file - this
parameter has no effect if MOD-LISP-P is NIL.

If INPUT-CHUNKING-P is true, the server will accept request bodies
without a `Content-Length' header if the client uses chunked transfer
encoding.  If you want to use this feature together with mod_lisp, you
should make sure that your combination of Apache and mod_lisp can do
that - see:

  <http://common-lisp.net/pipermail/mod-lisp-devel/2006-December/000104.html>.

On LispWorks 5.0 or higher and AllegroCL, READ-TIMEOUT and
WRITE-TIMEOUT are the read and write timeouts \(in seconds) of
the server - use NIL for no timeout at all.  (See the LispWorks
documentation for STREAM:SOCKET-STREAM for details.)  On
LispWorks 4.4.6 or lower, SBCL, and CMUCL WRITE-TIMEOUT is
ignored.  On OpenMCL both parameters are ignored.

On Unix you can use SETUID and SETGID to change the UID and GID of the
process directly after the server has been started.  \(You might want
to do this if you're using a privileged port like 80.)  SETUID and
SETGID can be integers \(the actual IDs) or strings \(for the user and
group name respectively).

If you want your server to use SSL you must provide the pathname
designator\(s) SSL-CERTIFICATE-FILE for the certificate file and
optionally SSL-PRIVATEKEY-FILE for the private key file, both files
must be in PEM format.  If you only provide the value for
SSL-CERTIFICATE-FILE it is assumed that both the certificate and the
private key are in one file.  If your private key needs a password you
can provide it through the SSL-PRIVATEKEY-PASSWORD keyword argument,
but this works only on LispWorks - for other Lisps the key must not be
associated with a password."
  (declare (ignorable port-provided-p))
  ;; initialize the session secret if needed
  (unless (boundp '*session-secret*)
    (reset-session-secret))
  (let ((output-chunking-p t))
    #-:hunchentoot-no-ssl
    (when ssl-certificate-file
      ;; disable output chunking for SSL connections
      (setq output-chunking-p nil)
      (unless port-provided-p (setq port 443)))
    ;; no timeouts if behind mod_lisp
    (when mod-lisp-p
      (setq read-timeout nil
            write-timeout nil))
    ;; use a new process/lock name for each server
    (atomic-incf *server-counter*)
    ;; create the SERVER object
    (let ((server (make-instance 'server
                                 :port port
                                 :address address
                                 :name name
                                 :dispatch-table dispatch-table
                                 :output-chunking-p (and output-chunking-p (not mod-lisp-p))
                                 :input-chunking-p input-chunking-p
                                 #-:hunchentoot-no-ssl #-:hunchentoot-no-ssl
                                 :ssl-certificate-file (and ssl-certificate-file
                                                            (namestring ssl-certificate-file))
                                 #-:hunchentoot-no-ssl #-:hunchentoot-no-ssl
                                 :ssl-privatekey-file (and ssl-privatekey-file
                                                           (namestring ssl-privatekey-file))
                                 #-:hunchentoot-no-ssl #-:hunchentoot-no-ssl
                                 :ssl-privatekey-password ssl-privatekey-password
                                 :mod-lisp-p mod-lisp-p
                                 :use-apache-log-p (and mod-lisp-p use-apache-log-p)
                                 :read-timeout read-timeout
                                 :write-timeout write-timeout)))
      (multiple-value-bind (process condition)
          ;; start up the actual server
          (start-up-server :service port
                           :address address
                           :process-name (format nil "hunchentoot-listener-~A" *server-counter*)
                           ;; this function is called once on
                           ;; startup - we use it to record the
                           ;; socket
                           :announce (lambda (socket &optional condition)
                                       (cond (socket
                                              (setf (server-socket server) socket))
                                             (condition
                                              (error condition))))
                           ;; this function is called whenever a
                           ;; connection is made
                           :function (lambda (handle)
                                       (with-lock ((server-lock server))
                                         (incf *worker-counter*)
                                         ;; check if we need to
                                         ;; perform a global GC
                                         (when (and *cleanup-interval*
                                                    (zerop (mod *worker-counter* *cleanup-interval*)))
                                           (when *cleanup-function*
                                             (funcall *cleanup-function*)))
                                         ;; start a worker thread
                                         ;; for this connection
                                         ;; and remember it
                                         (push (process-run-function (format nil "hunchentoot-worker-~A"
                                                                             *worker-counter*)
                                                                     #'process-connection
                                                                     server handle)
                                               (server-workers server))))
                           ;; wait until the server was
                           ;; successfully started or an error
                           ;; condition is returned
                           :wait t)
        (cond (process
               ;; remember the listener so we can kill it later
               (setf (server-listener server) process))
              (condition
               (error condition))))
      #+(and :unix (not :win32))
      (when setgid
        ;; we must make sure to call setgid before we call setuid or
        ;; suddenly we aren't root anymore...
        (etypecase setgid
          (integer (setgid setgid))
          (string (setgid (get-gid-from-name setgid)))))
      #+(and :unix (not :win32))
      (when setuid
        (etypecase setuid
          (integer (setuid setuid))
          (string (setuid (get-uid-from-name setuid)))))
      server)))

(defun stop-server (server)
  "Stops the Hunchentoot server SERVER."
  ;; use lock so that the listener can't start new workers
  (with-lock ((server-lock server))
    ;; kill all worker threads
    (dolist (worker (server-workers server))
      (ignore-errors (process-kill worker))
      (process-allow-scheduling))
    ;; finally, kill main listener
    (when-let (listener (server-listener server))
      (process-kill listener)))
  (values))

(defun process-connection (server handle)
  "This function is called by the server in a newly-created thread
with the SERVER object itself and a socket 'handle' from which a
stream can be created.  It reads the request headers and hands over to
PROCESS-REQUEST.  This is done in a loop until the stream has to be
closed or until a read timeout occurs."
  (handler-bind ((error
                  ;; abort if there's an error which isn't caught inside
                  (lambda (cond)
                    (log-message *lisp-errors-log-level*
                                 "Error while processing connection: ~A" cond)                    
                    (return-from process-connection)))
                 (warning
                  ;; log all warnings which aren't caught inside
                  (lambda (cond)
                    (log-message *lisp-warnings-log-level*
                                 "Warning while processing connection: ~A" cond))))
    (with-debugger
      (let (*hunchentoot-stream* *local-host* *remote-host* *remote-port*)
        (unwind-protect
            ;; bind important special variables
            (let ((*server* server))
              ;; create binary stream from socket handle
              (multiple-value-setq (*hunchentoot-stream* *local-host* *remote-host* *remote-port*)
                  (make-socket-stream handle
                                      (server-read-timeout server)
                                      (server-write-timeout server)))
              ;; attach SSL to the stream if necessary
              #-:hunchentoot-no-ssl
              (when (server-ssl-certificate-file server)
                #+:lispworks
                (make-ssl-server-stream *hunchentoot-stream*
                                        :certificate-file (server-ssl-certificate-file server)
                                        :privatekey-file (server-ssl-privatekey-file server)
                                        :privatekey-password (server-ssl-privatekey-password server))
                #-:lispworks
                (setq *hunchentoot-stream*
                        (cl+ssl:make-ssl-server-stream *hunchentoot-stream*
                                                       :certificate (server-ssl-certificate-file server)
                                                       :key (server-ssl-privatekey-file server))))
              ;; wrap with chunking-enabled stream if necessary
              (when (or (server-input-chunking-p server)
                        (server-output-chunking-p server))
                (setq *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)))
              ;; now wrap with flexi stream with "faithful" external format
              (setq *hunchentoot-stream*
                    (make-flexi-stream *hunchentoot-stream* :external-format +latin-1+))
              ;; loop until we have to close the stream - as
              ;; determined by *CLOSE-HUNCHENTOOT-STREAM*
              (unwind-protect                  
                  (loop
                   (let ((*close-hunchentoot-stream* t))
                     ;; reset to "faithful" format on each iteration
                     ;; and reset bound of stream as well
                     (setf (flexi-stream-external-format *hunchentoot-stream*) +latin-1+
                           (flexi-stream-bound *hunchentoot-stream*) nil)
                     (multiple-value-bind (headers-in content-stream method url-string server-protocol)
                         (get-request-data)
                       (unless (and ;; check if there was a request at all
                                    (cond ((server-mod-lisp-p server) headers-in)
                                          (t method))
                                    (prog1
                                        (process-request headers-in content-stream method
                                                         url-string server-protocol)
                                      ;; always turn chunking off at this point
                                      (when (or (server-input-chunking-p server)
                                                (server-output-chunking-p server))
                                        (setf (chunked-stream-output-chunking-p
                                               (flexi-stream-stream *hunchentoot-stream*)) nil
                                              (chunked-stream-input-chunking-p
                                               (flexi-stream-stream *hunchentoot-stream*)) nil))
                                      (force-output* *hunchentoot-stream*))
                                    ;; continue until we have to close
                                    ;; the stream
                                    (not *close-hunchentoot-stream*))
                         (return)))))
                (ignore-errors (force-output* *hunchentoot-stream*))))
          (when *hunchentoot-stream*
            (ignore-errors (close *hunchentoot-stream* :abort t)))
          (ignore-errors
            (with-lock ((server-lock server))
              ;; remove this worker from the list of all workers
              (setf (server-workers server)
                    (delete *current-process* (server-workers server))))))))))

(defun process-request (headers-in content-stream method url-string server-protocol)
  "This function is called by PROCESS-CONNECTION after the incoming
headers have been read.  It sets up the REQUEST and REPLY objects,
dispatches to a handler, and finally sends the output to the client
using START-OUTPUT.  If all goes as planned, the function returns T."
  (let (*tmp-files* *headers-sent*)
    (unwind-protect
        (progn
          (when (server-input-chunking-p *server*)
            (let ((transfer-encodings (cdr (assoc :transfer-encoding headers-in))))
              (when transfer-encodings
                (setq transfer-encodings
                      (split "\\s*,\\*" transfer-encodings)))
              (when (member "chunked" transfer-encodings :test #'equalp)
                ;; turn chunking on before we read the request body
                (setf (chunked-stream-input-chunking-p 
                       (flexi-stream-stream *hunchentoot-stream*)) t))))
          (let* ((*session* nil)
                 ;; first create a REPLY object so we can immediately start
                 ;; logging \(in case we're logging to mod_lisp)
                 (*reply* (make-instance 'reply))
                 (*request* (make-instance 'request
                                           :headers-in headers-in
                                           :content-stream content-stream
                                           :method method
                                           :uri url-string
                                           :server-protocol server-protocol))
                 (*dispatch-table* (or (server-dispatch-table *server*)
                                       (funcall *meta-dispatcher* *server*)))
                 backtrace)
            (multiple-value-bind (body error)
                (catch 'handler-done
                  (handler-bind ((error
                                  (lambda (cond)
                                    ;; only generate backtrace if needed
                                    (setq backtrace
                                          (and (or (and *show-lisp-errors-p*
                                                        *show-lisp-backtraces-p*)
                                                   (and *log-lisp-errors-p*
                                                        *log-lisp-backtraces-p*))
                                               (get-backtrace cond)))
                                    (when *log-lisp-errors-p*
                                      (log-message *lisp-errors-log-level*
                                                   "~A~:[~*~;~%~A~]"
                                                   cond
                                                   *log-lisp-backtraces-p*
                                                   backtrace))
                                    ;; if the headers were already sent
                                    ;; the error happens within the body
                                    ;; and we have to close the stream
                                    (when *headers-sent*
                                      (setq *close-hunchentoot-stream* t))
                                    (throw 'handler-done
                                           (values nil cond))))
                                 (warning
                                  (lambda (cond)
                                    (when *log-lisp-warnings-p*
                                      (log-message *lisp-warnings-log-level*
                                                   "~A~:[~*~;~%~A~]"
                                                   cond
                                                   *log-lisp-backtraces-p*
                                                   backtrace)))))
                    (with-debugger 
                      ;; skip dispatch if bad request
                      (when (eq (return-code) +http-ok+)
                        ;; now do the work
                        (dispatch-request *dispatch-table*)))))
              (when error
                (setf (return-code *reply*)
                      +http-internal-server-error+))
              (start-output (cond ((and error *show-lisp-errors-p*)
                                   (format nil "<pre>~A~:[~*~;~%~%~A~]</pre>"
                                           (escape-for-html (format nil "~A" error))
                                           *show-lisp-backtraces-p*
                                           (escape-for-html (format nil "~A" backtrace))))
                                  (error
                                   "An error has occured")
                                  (t body))))
            t))
      (dolist (path *tmp-files*)
        (when (and (pathnamep path) (probe-file path))
          (ignore-errors (delete-file path)))))))
