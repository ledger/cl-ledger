;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/headers.lisp,v 1.24 2007/09/24 13:43:45 edi Exp $

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

(defun maybe-write-to-header-stream (key &optional value)
  (when *header-stream*
    (format *header-stream* "~A~@[: ~A~]~%" key
            (and value (regex-replace-all "[\\r\\n]" value " ")))
    (force-output *header-stream*)))

(defun compute-length (content)
  "Computes and returns the length of CONTENT in octets.  Returns as a
second value CONTENT as a vector of octets.  The result depends on the
external format of *REPLY*."
  (when (null content)
    (return-from compute-length))
  (when (stringp content)
    (setq content
          (string-to-octets content :external-format (reply-external-format))))
  (values (length content) content))

(defmethod write-header-line ((mod-lisp-p (eql nil)) key value)
  "Accepts strings KEY and VALUE and writes them directly to the
client as an HTTP header line."
  (write-string key *hunchentoot-stream*)
  (write-string ": " *hunchentoot-stream*)
  ;; remove line breaks
  (write-string (regex-replace-all "[\\r\\n]" value " ") *hunchentoot-stream*)
  (write-string +crlf+ *hunchentoot-stream*))

(defmethod write-header-line (mod-lisp-p key value)
  "Accepts strings KEY and VALUE and writes them, one line at a time,
to the mod_lisp socket stream."
  (write-line key *hunchentoot-stream*)
  ;; remove line breaks
  (write-line (regex-replace-all "[\\r\\n]" value " ") *hunchentoot-stream*))

(defmethod write-header-line :after (mod-lisp-p key value)
  (declare (ignorable mod-lisp-p))
  (maybe-write-to-header-stream key value))

(defun start-output (&optional (content nil content-provided-p))
  "Sends all headers and maybe the content body to
*HUNCHENTOOT-STREAM*.  Returns immediately and does nothing if called
more than once per request.  Handles the supported return codes
accordingly.  Called by PROCESS-REQUEST and/or SEND-HEADERS.  Returns
the stream to write to."
  ;; send headers only once
  (when *headers-sent*
    (return-from start-output))
  (setq *headers-sent* t)
  ;; read post data to clear stream
  (raw-post-data)
  (let* ((mod-lisp-p (server-mod-lisp-p *server*))
         (return-code (return-code))
         (chunkedp (and (server-output-chunking-p *server*)
                        (eq (server-protocol) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length) content-provided-p))
                        ;; ...AND if the return code isn't one where
                        ;; Hunchentoot (or a user error handler) sends its
                        ;; own content
                        (member return-code *approved-return-codes*)))
         (reason-phrase (reason-phrase return-code))
         (request-method (request-method))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (unless mod-lisp-p
      (multiple-value-bind (keep-alive-p keep-alive-requested-p)
          (keep-alive-p)
        (when keep-alive-p
          (setq keep-alive-p
                ;; use keep-alive if there's a way for the client to
                ;; determine when all content is sent (or if there
                ;; is no content)
                (or chunkedp
                    head-request-p
                    (eq (return-code) +http-not-modified+)
                    (content-length)
                    content)))
        ;; now set headers for keep-alive and chunking
        (when chunkedp
          (setf (header-out "Transfer-Encoding") "chunked"))
        (cond (keep-alive-p
               (setf *close-hunchentoot-stream* nil)
               (when (or (not (eq (server-protocol) :http/1.1))
                         keep-alive-requested-p)
                 ;; persistent connections are implicitly assumed for
                 ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
                 ;; client has explicitly asked for one
                 (setf (header-out "Connection") "Keep-Alive"
                       (header-out "Keep-Alive")
                       (format nil "timeout=~D" (server-read-timeout *server*)))))
              (t (setf (header-out "Connection") "Close"))))
      (unless (and (header-out-set-p "Server")
                   (null (header-out "Server")))
        (setf (header-out "Server") (or (header-out "Server")
                                        (server-name-header))))
      (setf (header-out "Date") (rfc-1123-date)))
    (unless reason-phrase
      (setq content (escape-for-html
                     (format nil "Unknown http return code: ~A" return-code))
            content-modified-p t
            return-code +http-internal-server-error+
            reason-phrase (reason-phrase return-code)))
    (unless (or (not *handle-http-errors-p*)
                (member return-code *approved-return-codes*))
      ;; call error handler, if any - should return NIL if it can't
      ;; handle the error
      (let (error-handled-p)
        (when *http-error-handler*
          (setq error-handled-p (funcall *http-error-handler* return-code)
                content (or error-handled-p content)
                content-modified-p (or content-modified-p error-handled-p)))
        ;; handle common return codes other than 200, which weren't
        ;; handled by the error handler
        (unless error-handled-p
          (setf (content-type)
                "text/html; charset=iso-8859-1"
                content-modified-p t
                content
                (format nil "<html><head><title>~D ~A</title></head><body><h1>~:*~A</h1>~A<p><hr>~A</p></body></html>"
                        return-code reason-phrase
                        (case return-code
                          ((#.+http-internal-server-error+) content)
                          ((#.+http-moved-temporarily+ #.+http-moved-permanently+)
                           (format nil "The document has moved <a href='~A'>here</a>"
                                   (header-out "Location")))
                          ((#.+http-authorization-required+)
                           "The server could not verify that you are authorized to access the document requested.  Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't understand how to supply the credentials required.")
                          ((#.+http-forbidden+)
                           (format nil "You don't have permission to access ~A on this server."
                                   (script-name)))
                          ((#.+http-not-found+)
                           (format nil "The requested URL ~A was not found on this server."
                                   (script-name)))
                          ((#.+http-bad-request+)
                           "Your browser sent a request that this server could not understand.")
                          (otherwise ""))
                        (address-string))))))
    ;; start with status line      
    (cond (mod-lisp-p
           (write-header-line t "Status" (format nil "~D ~A" return-code reason-phrase)))
          (t
           (let ((first-line
                  (format nil "HTTP/1.1 ~D ~A" return-code reason-phrase)))
             (write-string first-line *hunchentoot-stream*)
             (write-string +crlf+ *hunchentoot-stream*)
             (maybe-write-to-header-stream first-line))))
    (when (and (stringp content)
               (not content-modified-p)
               (starts-with-one-of-p (or (content-type) "")
                                     *content-types-for-url-rewrite*))
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      (setq content (maybe-rewrite-urls-for-session content)))
    (let ((content-length (content-length)))
      (unless content-length
        (multiple-value-setq (content-length content) (compute-length content)))
      ;; write the corresponding headers for the content
      (when content-length
        (write-header-line mod-lisp-p "Content-Length" (format nil "~D" content-length))
        (when mod-lisp-p
          (write-header-line t "Lisp-Content-Length"
                             (cond (head-request-p "0")
                                   (t (format nil "~D" content-length))))
          (write-header-line t "Keep-Socket" "1")
          (setq *close-hunchentoot-stream* nil)))
      (when-let (content-type (content-type))
        (write-header-line mod-lisp-p "Content-Type" content-type))
      ;; write all headers from the REPLY object
      (loop for (key . value) in (headers-out)
            when value
            do (write-header-line mod-lisp-p (string-capitalize key) value))
      ;; now the cookies
      (loop for (nil . cookie) in (cookies-out)
            do (write-header-line mod-lisp-p "Set-Cookie" (stringify-cookie cookie)))
      (when mod-lisp-p
        ;; send log messages to mod_lisp
        (loop for (log-level . message) in (reverse (log-messages *reply*))
              do (write-header-line t (case log-level
                                        ((:emerg) "Log-Emerg")
                                        ((:alert) "Log-Alert")
                                        ((:crit) "Log-Crit")
                                        ((:error) "Log-Error")
                                        ((:warning) "Log-Warning")
                                        ((:notice) "Log-Notice")
                                        ((:info) "Log-Info")
                                        ((:debug) "Log-Debug")
                                        (otherwise "Log"))
                                    message)))
      ;; all headers sent
      (cond (mod-lisp-p
             (write-line "end" *hunchentoot-stream*)
             (maybe-write-to-header-stream "end"))
            (t
             (write-string +crlf+ *hunchentoot-stream*)
             (maybe-write-to-header-stream "")))
      ;; access log message
      (when (and *show-access-log-messages*
                 (not (server-use-apache-log-p *server*)))
        (ignore-errors
          (log-message nil "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] \"~A ~A~@[?~A~] ~A\" ~A ~:[~*-~;~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
                       (remote-addr) (header-in :x-forwarded-for)
                       (authorization) request-method (script-name)
                       (query-string) (server-protocol)
                       return-code content content-length
                       (referer) (user-agent)))))
    (setf (flexi-stream-external-format *hunchentoot-stream*) (reply-external-format))
    ;; now optional content
    (unless (or (null content) head-request-p)
      (ignore-errors
        (write-sequence content *hunchentoot-stream*)))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (setf (chunked-stream-output-chunking-p
             (flexi-stream-stream *hunchentoot-stream*)) t))
    *hunchentoot-stream*))

(defun send-headers ()
  "Sends the initial status line and all headers as determined by
the REPLY object *REPLY*.  Returns a stream to which the body of
the reply can be written.  Once this function has been called,
further changes to *REPLY* don't have any effect.  Also,
automatic handling of errors \(i.e. sending the corresponding
status code to the browser, etc.) is turned off for this request.
If your handlers return the full body as a string or as an array
of octets you should NOT call this function."
  (start-output))

(defun get-request-data ()
  "Reads incoming headers from mod_lisp or directly from the client
via *HUNCHENTOOT-STREAM*.  Returns as multiple values the headers as
an alist, the stream to read the request body from, the method, the
URI, and the protocol of the request.  The last three values are only
returned if we're not behind mod_lisp."
  (ignore-errors
    (let* ((mod-lisp-p (server-mod-lisp-p *server*))
           (first-line (if mod-lisp-p
                         (read-line *hunchentoot-stream* nil nil)
                         (cl:handler-case
                             (read-line* *hunchentoot-stream*)
                           ((or end-of-file
                                #+:sbcl sb-sys:io-timeout
                                #+:cmu sys:io-timeout
                                #+:allegro excl:socket-error) ()
                             nil)))))
      (cond ((null first-line)
             ;; socket closed - return immediately
             nil)
            (mod-lisp-p
             ;; we're behind mod_lisp, so we read alternating
             ;; key/value lines
             (let ((second-line (read-line *hunchentoot-stream* t)))
               (maybe-write-to-header-stream first-line second-line)
               (let* ((headers
                       (loop for key = (read-line *hunchentoot-stream* nil nil)
                             while (and key (string-not-equal key "end"))
                             for value = (read-line *hunchentoot-stream* t)
                             collect (cons (make-keyword key) value)
                             do (maybe-write-to-header-stream key value)))
                      (content-length (cdr (assoc :content-length headers))))
                 ;; add contents of first two lines
                 (push (cons (make-keyword first-line) second-line) headers)
                 (values (delete-duplicates headers :test #'eq :key #'car)
                         (and (or content-length
                                  (server-input-chunking-p *server*))
                              *hunchentoot-stream*)))))
            (t
             ;; we're a stand-alone web server, so we use Chunga to
             ;; read the headers
             (destructuring-bind (method url-string &optional protocol)
                 (split "\\s+" first-line :limit 3)
               (maybe-write-to-header-stream first-line)
               (let ((headers (and protocol (read-http-headers *hunchentoot-stream*
                                                               *header-stream*))))
                 (unless protocol (setq protocol "HTTP/0.9"))
                 (when (equalp (cdr (assoc :expect headers)) "100-continue")
                   ;; handle 'Expect: 100-continue' header
                   (let ((continue-line
                          (format nil "HTTP/1.1 ~D ~A"
                                  +http-continue+
                                  (reason-phrase +http-continue+))))
                     (write-string continue-line *hunchentoot-stream*)
                     (write-string +crlf+ *hunchentoot-stream*)
                     (write-string +crlf+ *hunchentoot-stream*)
                     (force-output *hunchentoot-stream*)
                     (maybe-write-to-header-stream continue-line)
                     (maybe-write-to-header-stream "")))
                 (values headers *hunchentoot-stream* (make-keyword method) url-string
                         (make-keyword (string-trim '(#\Space #\Tab #\NewLine #\Return) protocol))))))))))