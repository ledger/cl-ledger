;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/request.lisp,v 1.33 2007/09/14 12:12:33 edi Exp $

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

(defclass request ()
  ((headers-in :initarg :headers-in
               :documentation "An alist of the incoming headers.  Note
that these might be the headers coming in from mod_lisp which are
different from the headers sent by the client.")
   (method :initarg :method
           :documentation "The request method as a keyword.  This slot
is only filled if we're not behind mod_lisp.")
   (uri :initarg :uri
           :documentation "The request URI as a string.  This slot is
only filled if we're not behind mod_lisp.")
   (server-protocol :initarg :server-protocol
                    :documentation "The HTTP protocol as a keyword.
This slot is only filled if we're not behind mod_lisp.")
   (content-stream :initarg :content-stream
                   :reader content-stream
                   :documentation "A stream from which the request
body can be read if there is one.")
   (cookies-in :initform nil
               :documentation "An alist of the cookies sent by the client.")
   (get-parameters :initform nil
                   :documentation "An alist of the GET parameters sent
by the client.")
   (post-parameters :initform nil
                    :documentation "An alist of the POST parameters
sent by the client.")
   (script-name :initform nil
                :documentation "The URI requested by the client without
the query string.")
   (query-string :initform nil
                 :documentation "The query string of this request.")
   (session :initform nil
            :accessor session
            :documentation "The session object associated with this
request.")
   (aux-data :initform nil
             :accessor aux-data
             :documentation "Used to keep a user-modifiable alist with
arbitrary data during the request.")
   (raw-post-data :initform nil
                  :documentation "The raw string sent as the body of a
POST request, populated only if not a multipart/form-data request."))
  (:documentation "Objects of this class hold all the information
about an incoming request.  They are created automatically by
Hunchentoot and can be accessed by the corresponding handler."))

(defun parse-rfc2388-form-data (stream content-type-header)
  "Creates an alist of POST parameters from the stream STREAM which is
supposed to be of content type 'multipart/form-data'."
  (let* ((parsed-content-type-header (rfc2388:parse-header content-type-header :value))
	 (boundary (or (cdr (rfc2388:find-parameter
                             "BOUNDARY"
                             (rfc2388:header-parameters parsed-content-type-header)))
		       (return-from parse-rfc2388-form-data))))
    (loop for part in (rfc2388:parse-mime stream boundary)
          for headers = (rfc2388:mime-part-headers part)
          for content-disposition-header = (rfc2388:find-content-disposition-header headers)
          for name = (cdr (rfc2388:find-parameter
                           "NAME"
                           (rfc2388:header-parameters content-disposition-header)))
          when name
          collect (cons name
                        (let ((contents (rfc2388:mime-part-contents part)))
                          (if (pathnamep contents)
                            (list contents
                                  (rfc2388:get-file-name headers)
                                  (rfc2388:content-type part :as-string t))
                            contents))))))

(defun get-post-data (&key (request *request*) want-stream (already-read 0))
  "Reads the request body from the stream and stores the raw contents
\(as an array of octets) in the corresponding slot of the REQUEST
object.  Returns just the stream if WANT-STREAM is true.  If there's a
Content-Length header, it is assumed, that ALREADY-READ octets have
already been read."
  (let* ((headers-in (headers-in request))
         (content-length (when-let (content-length-header (cdr (assoc :content-length headers-in)))
                           (parse-integer content-length-header :junk-allowed t)))
         (content-stream (content-stream request)))
    (setf (slot-value request 'raw-post-data)
          (cond (want-stream
                 (setf (flexi-stream-position *hunchentoot-stream*) 0)
                 (when content-length
                   (setf (flexi-stream-bound content-stream) content-length))
                 content-stream)
                ((and content-length (> content-length already-read))
                 (decf content-length already-read)
                 (when (input-chunking-p)
                   ;; see RFC 2616, section 4.4
                   (log-message :warn "Got Content-Length header although input chunking is on."))
                 (let ((content (make-array content-length :element-type 'octet)))
                   (read-sequence content content-stream)
                   content))
                ((input-chunking-p)
                 (loop with buffer = (make-array +buffer-length+ :element-type 'octet)
                       with content = (make-array 0 :element-type 'octet :adjustable t)
                       for index = 0 then (+ index pos)
                       for pos = (read-sequence buffer content-stream)
                       do (adjust-array content (+ index pos))
                          (replace content buffer :start1 index :end2 pos)
                       while (= pos +buffer-length+)
                       finally (return content)))))))

(defmethod initialize-instance :after ((request request) &rest init-args)
  "The only initarg for a REQUEST object is :HEADERS-IN.  All other
slot values are computed in this :AFTER method."
  (declare (ignore init-args))
  (with-slots (headers-in cookies-in get-parameters post-parameters script-name query-string session)
      request
    (handler-case
        (progn
          (when (server-mod-lisp-p *server*)
            ;; convert these two values to keywords
            (let ((method-pair (assoc :method headers-in)))
              (setf (cdr method-pair) (make-keyword (cdr method-pair))))
            (let ((protocol-pair (assoc :server-protocol headers-in)))
              (setf (cdr protocol-pair) (make-keyword (cdr protocol-pair))))
            ;; and convert these two values to integers
            (let ((remote-ip-port-pair (assoc :remote-ip-port headers-in)))
              (setf (cdr remote-ip-port-pair) (parse-integer (cdr remote-ip-port-pair)
                                                             :junk-allowed t)))
            (let ((server-ip-port-pair (assoc :server-ip-port headers-in)))
              (setf (cdr server-ip-port-pair) (parse-integer (cdr server-ip-port-pair)
                                                             :junk-allowed t))))
          ;; compute SCRIPT-NAME and QUERY-STRING slots from
          ;; REQUEST_URI environment variable
          (let* ((uri (request-uri request))
                 (match-start (position #\? uri)))
            (cond
             (match-start
              (setq script-name (subseq uri 0 match-start)
                    query-string (subseq uri (1+ match-start))))
             (t (setq script-name uri))))
          ;; some clients (e.g. ASDF-INSTALL) send requests like
          ;; "GET http://server/foo.html HTTP/1.0"...
          (setq script-name (regex-replace "^https?://[^/]+" script-name ""))
          ;; compute GET parameters from query string and cookies from
          ;; the incoming 'Cookie' header
          (setq get-parameters
                (form-url-encoded-list-to-alist (split "&" query-string))
                cookies-in
                (form-url-encoded-list-to-alist (split "\\s*[,;]\\s*" (cdr (assoc :cookie headers-in)))
                                                +utf-8+)
                session (session-verify request)
                *session* session)
          ;; if the content-type is 'application/x-www-form-urlencoded'
          ;; or 'multipart/form-data', compute the post parameters from
          ;; the content body
          (when (member (request-method request) *methods-for-post-parameters* :test #'eq)
            (when-let (content-type (cdr (assoc :content-type headers-in)))
              (multiple-value-bind (type subtype external-format)
                  (parse-content-type content-type t)
                (setq post-parameters
                      (cond ((and (string-equal type "application")
                                  (string-equal subtype "x-www-form-urlencoded"))
                             (unless (or (assoc :content-length headers-in)
                                         (input-chunking-p))
                               (error "Can't read request body because there's no~
Content-Length header and input chunking is off."))
                             (form-url-encoded-list-to-alist
                              (split "&" (raw-post-data :request request
                                                        ;; ASCII would suffice according to RFC...
                                                        :external-format +latin-1+))
                              external-format))
                            ((and (string-equal type "multipart")
                                  (string-equal subtype "form-data"))
                             (setf (slot-value request 'raw-post-data) t)                           
                             (handler-case
                                 (let* ((*request* request)
                                        (content-stream (content-stream request))
                                        (start (flexi-stream-position content-stream)))
                                   (prog1
                                       (parse-rfc2388-form-data content-stream content-type)
                                     (let* ((end (flexi-stream-position content-stream))
                                            (stray-data (get-post-data :already-read (- end start))))
                                       (when (and stray-data (plusp (length stray-data)))
                                         (warn "~A octets of stray data after form-data sent by client."
                                               (length stray-data))))
                                     (setf (slot-value request 'raw-post-data) t)))
                               (error (msg)
                                 (log-message :error
                                              "While parsing multipart/form-data parameters: ~A"
                                              msg)
                                 nil)))))))))
      (error (cond)
        (log-message* "Error when creating REQUEST object: ~A" cond)
        ;; we assume it's not our fault...
        (setf (return-code) +http-bad-request+)))))

(defun recompute-request-parameters (&key (request *request*)
                                          (external-format *hunchentoot-default-external-format*))
  "Recomputes the GET and POST parameters for the REQUEST object
REQUEST.  This only makes sense if you're switching external formats
during the request."
  (with-slots (headers-in get-parameters post-parameters query-string)
      request
    (setq get-parameters
          (form-url-encoded-list-to-alist (split "&" query-string) external-format)
          post-parameters
          (when-let (raw-post-data (raw-post-data :request request
                                                  :external-format +latin-1+))
            (and (when-let (content-type (cdr (assoc :content-type headers-in)))
                   (multiple-value-bind (type subtype)
                       (parse-content-type content-type)
                     (and (string-equal type "application")
                          (string-equal subtype "x-www-form-urlencoded"))))
                 (form-url-encoded-list-to-alist (split "&" raw-post-data) external-format)))))
  (values))
                                                
(defun script-name (&optional (request *request*))
  "Returns the file name of the REQUEST object REQUEST. That's the
requested URI without the query string \(i.e the GET parameters)."
  (slot-value request 'script-name))

(defun query-string (&optional (request *request*))
  "Returns the query string of the REQUEST object REQUEST. That's
the part behind the question mark \(i.e. the GET parameters)."
  (slot-value request 'query-string))

(defun get-parameters (&optional (request *request*))
  "Returns an alist of the GET parameters associated with the REQUEST
object REQUEST."
  (slot-value request 'get-parameters))

(defun post-parameters (&optional (request *request*))
  "Returns an alist of the POST parameters associated with the REQUEST
object REQUEST."
  (slot-value request 'post-parameters))

(defun headers-in (&optional (request *request*))
  "Returns an alist of the incoming headers associated with the
REQUEST object REQUEST."
  (slot-value request 'headers-in))

(defun cookies-in (&optional (request *request*))
  "Returns an alist of all cookies associated with the REQUEST object
REQUEST."
  (slot-value request 'cookies-in))

(defun header-in (name &optional (request *request*))
  "Returns the incoming header with name NAME.  NAME can be a keyword
\(recommended) or a string."
  (cdr (assoc name (headers-in request))))

(defun authorization (&optional (request *request*))
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (header-in :authorization request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (split ":" (base64:base64-string-to-string (subseq authorization start)))
        (values user password)))))

(defun remote-addr (&optional (request *request*))
  "Returns the address the current request originated from."
  (cond ((server-mod-lisp-p *server*) (header-in :remote-ip-addr request))
        (t *remote-host*)))

(defun real-remote-addr (&optional (request *request*))
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (header-in :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-addr request)))))

(defun server-addr (&optional (request *request*))
  "Returns the address at which the current request arrived."
  (cond ((server-mod-lisp-p *server*) (header-in :server-ip-addr request))
        (t *local-host*)))

(defun remote-port (&optional (request *request*))
  "Returns the port the current request originated from."
  (cond ((server-mod-lisp-p *server*) (header-in :remote-ip-port request))
        (t *remote-port*)))

(defun server-port (&optional (request *request*))
  "Returns the port at which the current request arrived."
  (cond ((server-mod-lisp-p *server*) (header-in :server-ip-port request))
        (t (server-local-port *server*))))

(defun host (&optional (request *request*))
  "Returns the 'Host' incoming http header value."
  (header-in :host request))

(defun request-uri (&optional (request *request*))
  "Returns the request URI."
  (cond ((server-mod-lisp-p *server*) (header-in :url request))
        (t (slot-value request 'uri))))

(defun request-method (&optional (request *request*))
  "Returns the request method as a Lisp keyword."
  (cond ((server-mod-lisp-p *server*) (header-in :method request))
        (t (slot-value request 'method))))

(defun server-protocol (&optional (request *request*))
  "Returns the request protocol as a Lisp keyword."
  (cond ((server-mod-lisp-p *server*) (header-in :server-protocol request))
        (t (slot-value request 'server-protocol))))

(defun mod-lisp-id (&optional (request *request*))
  "Returns the 'Server ID' sent by mod_lisp.  This value is set in
Apache's server configuration file and is of course only available if
mod_lisp is the front-end."
  (and (or (server-mod-lisp-p *server*)
           (warn "Calling MOD-LISP-ID although ~S is a stand-alone server."
                 *server*))
       (header-in :server-id request)))

(defun ssl-session-id (&optional (request *request*))
  "Returns the 'SSL_SESSION_ID' header sent my mod_lisp and is of
course only available if mod_lisp is the front-end."
  (and (or (server-mod-lisp-p *server*)           
           (warn "Calling SSL-SESSION-ID although ~S is a stand-alone server."
                 *server*))
       (header-in :ssl-session-id request)))

(defun user-agent (&optional (request *request*))
  "Returns the 'User-Agent' http header."
  (header-in :user-agent request))

(defun cookie-in (name &optional (request *request*))
  "Returns the cookie with the name NAME \(a string) as sent by the
browser - or NIL if there is none."
  (cdr (assoc name (cookies-in request) :test #'string=)))

(defun referer (&optional (request *request*))
  "Returns the 'Referer' \(sic!) http header."
  (header-in :referer request))

(defun get-parameter (name &optional (request *request*))
  "Returns the GET parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (get-parameters request) :test #'string=)))

(defun post-parameter (name &optional (request *request*))
  "Returns the POST parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (post-parameters request) :test #'string=)))

(defun parameter (name &optional (request *request*))
  "Returns the GET or the POST parameter with name NAME \(a string) -
or NIL if there is none.  If both a GET and a POST parameter with the
same name exist the GET parameter is returned.  Search is
case-sensitive."
  (or (get-parameter name request)
      (post-parameter name request)))

(defun handle-if-modified-since (time &optional (request *request*))
  "Handles the 'If-Modified-Since' header of REQUEST.  The date string
is compared to the one generated from the supplied universal time
TIME."
  (let ((if-modified-since (header-in :if-modified-since request))
        (time-string (rfc-1123-date time)))
    ;; simple string comparison is sufficient; see RFC 2616 14.25
    (when (and if-modified-since
               (equal if-modified-since time-string))
      (setf (return-code) +http-not-modified+)
      (throw 'handler-done nil))
    (values)))

(defun raw-post-data (&key (request *request*) external-format force-binary force-text want-stream)
  "Returns the content sent by the client if there was any \(unless
the content type was \"multipart/form-data\").  By default, the result
is a string if the type of the `Content-Type' media type is \"text\",
and a vector of octets otherwise.  In the case of a string, the
external format to be used to decode the content will be determined
from the `charset' parameter sent by the client \(or otherwise
*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT* will be used).

You can also provide an external format explicitly \(through
EXTERNAL-FORMAT) in which case the result will unconditionally be a
string.  Likewise, you can provide a true value for FORCE-TEXT which
will force Hunchentoot to act as if the type of the media type had
been \"text\".  Or you can provide a true value for FORCE-BINARY which
means that you want a vector of octets at any rate.

If, however, you provide a true value for WANT-STREAM, the other
parameters are ignored and you'll get the content \(flexi) stream to
read from it yourself.  It is then your responsibility to read the
correct amount of data, because otherwise you won't be able to return
a response to the client.  If the content type of the request was
`multipart/form-data' or `application/x-www-form-urlencoded', the
content has been read by Hunchentoot already and you can't read from
the stream anymore.

You can call RAW-POST-DATA more than once per request, but you can't
mix calls which have different values for WANT-STREAM.

Note that this function is slightly misnamed because a client can send
content even if the request method is not POST."
  (when (and force-binary force-text)
    (error "It doesn't make sense to set both FORCE-BINARY and FORCE-TEXT to a true value."))
  (unless (or external-format force-binary)
    (setq external-format
          (when-let (content-type (cdr (assoc :content-type (headers-in request))))
            (nth-value 2 (parse-content-type content-type force-text)))))
  (let ((raw-post-data (or (slot-value request 'raw-post-data)
                           (get-post-data :request request :want-stream want-stream))))
    (cond ((typep raw-post-data 'stream) raw-post-data)
          ((member raw-post-data '(t nil)) nil)
          (external-format (octets-to-string raw-post-data :external-format external-format))
          (t raw-post-data))))

(defun aux-request-value (symbol &optional (request *request*))
  "Returns the value associated with SYMBOL from the request object
REQUEST \(the default is the current request) if it exists.  The
second return value is true if such a value was found."
  (when request
    (let ((found (assoc symbol (aux-data request))))
      (values (cdr found) found))))

(defsetf aux-request-value (symbol &optional request)
    (new-value)
  "Sets the value associated with SYMBOL from the request object
REQUEST \(default is *REQUEST*).  If there is already a value
associated with SYMBOL it will be replaced."
  (with-rebinding (symbol)
    (with-unique-names (place %request)
      `(let* ((,%request (or ,request *request*))
              (,place (assoc ,symbol (aux-data ,%request))))
         (cond
           (,place
            (setf (cdr ,place) ,new-value))
           (t
            (push (cons ,symbol ,new-value)
                  (aux-data ,%request))
            ,new-value))))))

(defun delete-aux-request-value (symbol &optional (request *request*))
  "Removes the value associated with SYMBOL from the request object
REQUEST."
  (when request
    (setf (aux-data request)
            (delete symbol (aux-data request)
                    :key #'car :test #'eq)))
  (values))
