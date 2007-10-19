;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

(in-package :cl+ssl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ssl-error-none+ 0)
  (defconstant +ssl-error-ssl+ 1)
  (defconstant +ssl-error-want-read+ 2)
  (defconstant +ssl-error-want-write+ 3)
  (defconstant +ssl-error-want-x509-lookup+ 4)
  (defconstant +ssl-error-syscall+ 5)
  (defconstant +ssl-error-zero-return+ 6)
  (defconstant +ssl-error-want-connect+ 7))


;;; Condition hierarchy
;;;
(define-condition ssl-error (error)
  ((queue :initform nil :initarg :queue :reader ssl-error-queue)))

(define-condition ssl-error/handle (ssl-error)
  ((ret :initarg :ret
        :reader ssl-error-ret)
   (handle :initarg :handle
           :reader ssl-error-handle))
  (:report (lambda (condition stream)
             (format stream "Unspecified error ~A on handle ~A" 
                     (ssl-error-ret condition)
                     (ssl-error-handle condition))
	     (write-sequence (ssl-error-queue condition) stream))))

(define-condition ssl-error-initialize (ssl-error)
  ((reason  :initarg :reason
            :reader ssl-error-reason))
  (:report (lambda (condition stream)
             (format stream "SSL initialization error: ~A"
                     (ssl-error-reason condition))
	     (write-sequence (ssl-error-queue condition) stream))))


(define-condition ssl-error-want-something (ssl-error/handle)
  ())

;;;SSL_ERROR_NONE
(define-condition ssl-error-none (ssl-error/handle)
  ()
  (:documentation
   "The TLS/SSL I/O operation completed. This result code is returned if and
    only if ret > 0.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A completed. (return code:  ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	     (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_ZERO_RETURN
(define-condition ssl-error-zero-return (ssl-error/handle)
  ()
  (:documentation
   "The TLS/SSL connection has been closed. If the protocol version is SSL 3.0
    or TLS 1.0, this result code is returned only if a closure alert has
    occurred in the protocol, i.e. if the connection has been closed cleanly.
    Note that in this case SSL_ERROR_ZERO_RETURN
    does not necessarily indicate that the underlying transport has been
    closed.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL connection on handle ~A has been closed. (return code:  ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	     (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_WANT_READ
(define-condition ssl-error-want-read (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for 
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a READ. (return code:  ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	     (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_WANT_WRITE
(define-condition ssl-error-want-write (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for 
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a WRITE. (return code:  ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	     (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_WANT_CONNECT
(define-condition ssl-error-want-connect (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. The underlying BIO was not connected yet to the peer
    and the call would block in connect()/accept(). The SSL
    function should be called again when the connection is established. These
    messages can only appear with a BIO_s_connect() or
    BIO_s_accept() BIO, respectively. In order to find out, when
    the connection has been successfully established, on many platforms
    select() or poll() for writing on the socket file
    descriptor can be used.")
  (:report (lambda (condition stream)
            (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a connect first. (return code:  ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	    (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_WANT_X509_LOOKUP
(define-condition ssl-error-want-x509-lookup (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete because an application callback set by
    SSL_CTX_set_client_cert_cb() has asked to be called again. The
    TLS/SSL I/O function should be called again later. Details depend on the
    application.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: An application callback wants to be called again. (return code:  ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	     (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_SYSCALL
(define-condition ssl-error-syscall (ssl-error/handle)
  ((syscall :initarg :syscall))
  (:documentation
   "Some I/O error occurred. The OpenSSL error queue may contain more
    information on the error. If the error queue is empty (i.e. ERR_get_error() returns 0),
    ret can be used to find out more about the error: If ret == 0, an EOF was observed that
    violates the protocol. If ret == -1, the underlying BIO reported an I/O error (for socket
    I/O on Unix systems, consult errno for details).")
  (:report (lambda (condition stream)
             (if (zerop (err-get-error))
                 (case (ssl-error-ret condition)
                   (0 (format stream "An I/O error occurred: An unexpected EOF was observed on handle ~A. (return code:  ~A)"
                              (ssl-error-handle condition)
                              (ssl-error-ret condition)))
                   (-1 (format stream "An I/O error occurred in the underlying BIO. (return code:  ~A)"
                               (ssl-error-ret condition)))
                   (otherwise (format stream "An I/O error occurred: undocumented reason. (return code:  ~A)"
                                      (ssl-error-ret condition))))
                 (format stream "An UNKNOWN I/O error occurred in the underlying BIO. (return code:  ~A)"
                         (ssl-error-ret condition)))
	     (write-sequence (ssl-error-queue condition) stream))))

;; SSL_ERROR_SSL
(define-condition ssl-error-ssl (ssl-error/handle)
  ()
  (:documentation
   "A failure in the SSL library occurred, usually a protocol error. The
    OpenSSL error queue contains more information on the error.")
  (:report (lambda (condition stream)
             (format stream
		     "A failure in the SSL library occurred on handle ~A. (Return code: ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
	     (write-sequence (ssl-error-queue condition) stream))))

(defun write-ssl-error-queue (stream)
  (format stream "SSL error queue: ~%")
  (loop
      for error-code = (err-get-error)
      until (zerop error-code)
      do (format stream "~a~%" (err-error-string error-code (cffi:null-pointer)))))

(defun ssl-signal-error (handle syscall error-code original-error)
  (let ((queue (with-output-to-string (s) (write-ssl-error-queue s))))
    (if (and (eql error-code #.+ssl-error-syscall+)
	     (not (zerop original-error)))
	(error 'ssl-error-syscall
	       :handle handle
	       :ret error-code
	       :queue queue
	       :syscall syscall)
      (error (case error-code
	       (#.+ssl-error-none+ 'ssl-error-none)
	       (#.+ssl-error-ssl+ 'ssl-error-ssl)
	       (#.+ssl-error-want-read+ 'ssl-error-want-read)
	       (#.+ssl-error-want-write+ 'ssl-error-want-write)
	       (#.+ssl-error-want-x509-lookup+ 'ssl-error-want-x509-lookup)
	       (#.+ssl-error-zero-return+ 'ssl-error-zero-return)
	       (#.+ssl-error-want-connect+ 'ssl-error-want-connect)
	       (#.+ssl-error-syscall+ 'ssl-error-zero-return)
	       (t 'ssl-error/handle))
	     :handle handle
	     :ret error-code
	     :queue queue))))
