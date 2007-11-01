;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/stream.lisp,v 1.50 2007/09/06 23:46:29 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :flexi-streams)

(defclass flexi-stream (trivial-gray-stream-mixin)
  ((stream :initarg :stream
           :reader flexi-stream-stream
           :documentation "The actual stream that's used for
input and/or output.  It must be capable of reading/writing
octets with READ-SEQUENCE and/or WRITE-SEQUENCE.")
   (external-format :initform (make-external-format :iso-8859-1)
                    :initarg :flexi-stream-external-format
                    :accessor flexi-stream-external-format
                    :documentation "The encoding currently used
by this stream.  Can be changed on the fly.")
   (element-type :initform #+:lispworks 'lw:simple-char #-:lispworks 'character
                 :initarg :element-type
                 :accessor flexi-stream-element-type
                 :documentation "The element type of this stream."))
  (:documentation "A FLEXI-STREAM object is a stream that's
`layered' atop an existing binary/bivalent stream in order to
allow for multi-octet external formats.  FLEXI-STREAM itself is a
mixin and should not be instantiated."))

(define-condition flexi-stream-error (stream-error)
  ()
  (:documentation "Superclass for all errors related to
flexi streams."))

(define-condition flexi-stream-simple-error (flexi-stream-error simple-condition)
  ()
  (:documentation "Like FLEXI-STREAM-ERROR but with formatting
capabilities."))

(define-condition flexi-stream-element-type-error (flexi-stream-error)
  ((element-type :initarg :element-type
                 :reader flexi-stream-element-type-error-element-type))
  (:report (lambda (condition stream)
             (format stream "Element type ~S not allowed."
                     (flexi-stream-element-type-error-element-type condition))))
  (:documentation "Errors of this type are signaled if the flexi
stream has a wrong element type."))

(define-condition flexi-stream-encoding-error (flexi-stream-simple-error)
  ()
  (:documentation "Errors of this type are signaled if there is an
encoding problem."))

(define-condition flexi-stream-position-spec-error (flexi-stream-simple-error)
  ((position-spec :initarg :position-spec
                  :reader flexi-stream-position-spec-error-position-spec))
  (:documentation "Errors of this type are signaled if an
erroneous position spec is used in conjunction with
FILE-POSITION."))

(defun signal-encoding-error (flexi-stream format-control &rest format-args)
  "Convenience function similar to ERROR to signal conditions of type
FLEXI-STREAM-ENCODING-ERROR."
  (error 'flexi-stream-encoding-error
         :format-control format-control
         :format-arguments format-args
         :stream flexi-stream))

(defun maybe-convert-external-format (external-format)
  "Given an external format designator \(a keyword, a list, or an
EXTERNAL-FORMAT object) returns the corresponding EXTERNAL-FORMAT
object."
  (typecase external-format
    (symbol (make-external-format external-format))
    (list (apply #'make-external-format external-format))
    (otherwise external-format)))

(defmethod initialize-instance :after ((flexi-stream flexi-stream) &rest initargs)
  "Makes sure the EXTERNAL-FORMAT and ELEMENT-TYPE slots contain
reasonable values."
  (declare (ignore initargs)
           (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format)
                   (element-type flexi-stream-element-type))
      flexi-stream
    (unless (or (subtypep element-type 'character)
                (subtypep element-type 'octet))
      (error 'flexi-stream-element-type-error
             :element-type element-type
             :stream flexi-stream))
    (setq external-format (maybe-convert-external-format external-format)))
  ;; set actual class and maybe contents of 8-bit encoding slots
  (set-class flexi-stream))

(defmethod (setf flexi-stream-external-format) :around (new-value (flexi-stream flexi-stream))
  "Converts the new value to an EXTERNAL-FORMAT object if
necessary."
  (call-next-method (maybe-convert-external-format new-value) flexi-stream))

(defmethod (setf flexi-stream-element-type) :before (new-value (flexi-stream flexi-stream))
  "Checks whether the new value makes sense before it is set."
  (unless (or (subtypep new-value 'character)
              (subtypep new-value 'octet))
    (error 'flexi-stream-element-type-error
           :element-type new-value
           :stream flexi-stream)))

(defmethod stream-element-type ((stream flexi-stream))
  "Returns the element type that was provided by the creator of
the stream."
  (declare (optimize speed))
  (flexi-stream-element-type stream))

(defmethod close ((stream flexi-stream) &key abort)
  "Closes the flexi stream by closing the underlying `real'
stream."
  (declare (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      stream
    (cond ((open-stream-p stream)
           (close stream :abort abort))
          (t nil))))

(defmethod open-stream-p ((stream flexi-stream))
  "A flexi stream is open if its underlying stream is open."
  (declare (optimize speed))
  (open-stream-p (flexi-stream-stream stream)))

(defmethod stream-file-position ((stream flexi-stream))
  "Dispatch to method for underlying stream."
  (declare (optimize speed))
  (stream-file-position (flexi-stream-stream stream)))

(defmethod (setf stream-file-position) (position-spec (stream flexi-stream))
  "Dispatch to method for underlying stream."
  (declare (optimize speed))
  (setf (stream-file-position (flexi-stream-stream stream))
        position-spec))

(defclass flexi-output-stream (flexi-stream fundamental-binary-output-stream
                                            fundamental-character-output-stream)
  ((column :initform 0
           :accessor flexi-stream-column
           :documentation "The current output column.  A
non-negative integer or NIL."))
  (:documentation "A FLEXI-OUTPUT-STREAM is a FLEXI-STREAM that
can actually be instatiated and used for output.  Don't use
MAKE-INSTANCE to create a new FLEXI-OUTPUT-STREAM but use
MAKE-FLEXI-STREAM instead."))

#+:cmu
(defmethod input-stream-p ((stream flexi-output-stream))
  "Explicitly states whether this is an input stream."
  (declare (optimize speed))
  nil)

(defclass flexi-input-stream (flexi-stream fundamental-binary-input-stream
                                           fundamental-character-input-stream)
  ((last-char-code :initform nil
                   :accessor flexi-stream-last-char-code
                   :documentation "This slot either holds NIL or the
last character \(code) read successfully.  This is mainly used for
UNREAD-CHAR sanity checks.")
   (last-octet :initform nil
               :accessor flexi-stream-last-octet
               :documentation "This slot either holds NIL or the last
octet read successfully from the stream using a `binary' operation
such as READ-BYTE.  This is mainly used for UNREAD-BYTE sanity
checks.")
   (octet-stack :initform nil
                :accessor flexi-stream-octet-stack
                :documentation "A small buffer which holds octets
that were already read from the underlying stream but not yet
used to produce characters.  This is mainly used if we have to
look ahead for a CR/LF line ending.")
   (position :initform 0
             :initarg :position
             :type integer
             :accessor flexi-stream-position
             :documentation "The position within the stream where each
octet read counts as one.")
   (bound :initform nil
          :initarg :bound
          :type (or null integer)
          :accessor flexi-stream-bound
          :documentation "When this is not NIL, it must be an integer
and the stream will behave as if no more data is available as soon as
POSITION is greater or equal than this value."))
  (:documentation "A FLEXI-INPUT-STREAM is a FLEXI-STREAM that
can actually be instatiated and used for input.  Don't use
MAKE-INSTANCE to create a new FLEXI-INPUT-STREAM but use
MAKE-FLEXI-STREAM instead."))

#+:cmu
(defmethod output-stream-p ((stream flexi-input-stream))
  "Explicitly states whether this is an output stream."
  (declare (optimize speed))
  nil)

(defclass flexi-io-stream (flexi-input-stream flexi-output-stream)
  ()
  (:documentation "A FLEXI-IO-STREAM is a FLEXI-STREAM that can
actually be instatiated and used for input and output.  Don't use
MAKE-INSTANCE to create a new FLEXI-IO-STREAM but use
MAKE-FLEXI-STREAM instead."))

(defclass flexi-cr-mixin ()
  ()
  (:documentation "A mixin for flexi streams which need
end-of-line conversion, i.e. for those where the end-of-line
designator is /not/ the single character #\Linefeed."))

(defclass flexi-8-bit-input-stream (flexi-input-stream)
  ((encoding-table :accessor flexi-stream-encoding-table))
  (:documentation "The class for all flexi input streams which
use an 8-bit encoding and thus need an additional slot for the
encoding table."))

(defclass flexi-cr-8-bit-input-stream (flexi-cr-mixin flexi-8-bit-input-stream)
  ()
  (:documentation "The class for all flexi input streams which
use an 8-bit encoding /and/ need end-of-line conversion."))

(defclass flexi-ascii-input-stream (flexi-8-bit-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the US-ASCCI encoding."))

(defclass flexi-cr-ascii-input-stream (flexi-cr-mixin flexi-ascii-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the US-ASCCI encoding /and/ need end-of-line conversion."))

(defclass flexi-latin-1-input-stream (flexi-8-bit-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the ISO-8859-1 encoding."))

(defclass flexi-cr-latin-1-input-stream (flexi-cr-mixin flexi-latin-1-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the ISO-8859-1 encoding /and/ need end-of-line conversion."))

(defclass flexi-utf-32-le-input-stream (flexi-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-32 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-32-le-input-stream (flexi-cr-mixin flexi-utf-32-le-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-32 encoding with little-endian byte ordering /and/
need end-of-line conversion."))

(defclass flexi-utf-32-be-input-stream (flexi-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-32 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-32-be-input-stream (flexi-cr-mixin flexi-utf-32-be-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-32 encoding with big-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-16-le-input-stream (flexi-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-16 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-16-le-input-stream (flexi-cr-mixin flexi-utf-16-le-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-16 encoding with little-endian byte ordering /and/
need end-of-line conversion."))

(defclass flexi-utf-16-be-input-stream (flexi-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-16 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-16-be-input-stream (flexi-cr-mixin flexi-utf-16-be-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-16 encoding with big-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-8-input-stream (flexi-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-8 encoding."))

(defclass flexi-cr-utf-8-input-stream (flexi-cr-mixin flexi-utf-8-input-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the UTF-8 encoding /and/ need end-of-line conversion."))

(defclass flexi-8-bit-output-stream (flexi-output-stream)
  ((encoding-hash :accessor flexi-stream-encoding-hash))
  (:documentation "The class for all flexi output streams which
use an 8-bit encoding and thus need an additional slot for the
encoding table."))

(defclass flexi-cr-8-bit-output-stream (flexi-cr-mixin flexi-8-bit-output-stream)
  ()
  (:documentation "The class for all flexi output streams which
use an 8-bit encoding /and/ need end-of-line conversion."))

(defclass flexi-ascii-output-stream (flexi-8-bit-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the US-ASCCI encoding."))

(defclass flexi-cr-ascii-output-stream (flexi-cr-mixin flexi-ascii-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the US-ASCCI encoding /and/ need end-of-line conversion."))

(defclass flexi-latin-1-output-stream (flexi-8-bit-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the ISO-8859-1 encoding."))

(defclass flexi-cr-latin-1-output-stream (flexi-cr-mixin flexi-latin-1-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the ISO-8859-1 encoding /and/ need end-of-line conversion."))

(defclass flexi-utf-32-le-output-stream (flexi-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-32 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-32-le-output-stream (flexi-cr-mixin flexi-utf-32-le-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-32 encoding with little-endian byte ordering /and/
need end-of-line conversion."))

(defclass flexi-utf-32-be-output-stream (flexi-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-32 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-32-be-output-stream (flexi-cr-mixin flexi-utf-32-be-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-32 encoding with big-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-16-le-output-stream (flexi-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-16 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-16-le-output-stream (flexi-cr-mixin flexi-utf-16-le-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-16 encoding with little-endian byte ordering /and/
need end-of-line conversion."))

(defclass flexi-utf-16-be-output-stream (flexi-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-16 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-16-be-output-stream (flexi-cr-mixin flexi-utf-16-be-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-16 encoding with big-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-8-output-stream (flexi-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-8 encoding."))

(defclass flexi-cr-utf-8-output-stream (flexi-cr-mixin flexi-utf-8-output-stream)
  ()
  (:documentation "Special class for flexi output streams which
use the UTF-8 encoding /and/ need end-of-line conversion."))

(defclass flexi-8-bit-io-stream (flexi-8-bit-input-stream flexi-8-bit-output-stream flexi-io-stream)
  ()
  (:documentation "The class for all flexi I/O streams which use
an 8-bit encoding and thus need an additional slot for the
encoding table."))
  
(defclass flexi-cr-8-bit-io-stream (flexi-cr-mixin flexi-8-bit-io-stream)
  ()
  (:documentation "The class for all flexi I/O streams which use
an 8-bit encoding /and/ need end-of-line conversion."))

(defclass flexi-ascii-io-stream (flexi-ascii-input-stream flexi-ascii-output-stream flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the US-ASCCI encoding."))

(defclass flexi-cr-ascii-io-stream (flexi-cr-mixin flexi-ascii-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the US-ASCCI encoding /and/ need end-of-line conversion."))

(defclass flexi-latin-1-io-stream (flexi-latin-1-input-stream flexi-latin-1-output-stream flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the ISO-8859-1 encoding."))

(defclass flexi-cr-latin-1-io-stream (flexi-cr-mixin flexi-latin-1-io-stream)
  ()
  (:documentation "Special class for flexi input streams which
use the ISO-8859-1 encoding /and/ need end-of-line conversion."))

(defclass flexi-utf-32-le-io-stream (flexi-utf-32-le-input-stream
                                     flexi-utf-32-le-output-stream
                                     flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-32 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-32-le-io-stream (flexi-cr-mixin flexi-utf-32-le-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-32 encoding with little-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-32-be-io-stream (flexi-utf-32-be-input-stream
                                     flexi-utf-32-be-output-stream
                                     flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-32 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-32-be-io-stream (flexi-cr-mixin flexi-utf-32-be-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-32 encoding with big-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-16-le-io-stream (flexi-utf-16-le-input-stream
                                     flexi-utf-16-le-output-stream
                                     flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-16 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-16-le-io-stream (flexi-cr-mixin flexi-utf-16-le-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-16 encoding with little-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-16-be-io-stream (flexi-utf-16-be-input-stream
                                     flexi-utf-16-be-output-stream
                                     flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-16 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-16-be-io-stream (flexi-cr-mixin flexi-utf-16-be-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-16 encoding with big-endian byte ordering /and/ need
end-of-line conversion."))

(defclass flexi-utf-8-io-stream (flexi-utf-8-input-stream flexi-utf-8-output-stream flexi-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-8 encoding."))

(defclass flexi-cr-utf-8-io-stream (flexi-cr-mixin flexi-utf-8-io-stream)
  ()
  (:documentation "Special class for flexi I/O streams which use
the UTF-8 encoding /and/ need end-of-line conversion."))

(defmethod (setf flexi-stream-external-format) :after (new-value (stream flexi-stream))
  "After we've changed the external format of a flexi stream, we
might have to change its actual class and maybe also the contents
of its 8-bit encoding slots."
  (declare (ignore new-value)
           (optimize speed))
  ;; note that it's potentially dangerous to call SET-CLASS from
  ;; within a method, see for example this thread:
  ;; <http://thread.gmane.org/gmane.lisp.lispworks.general/6269>
  (set-class stream))

(defmethod set-class ((stream flexi-input-stream))
  "Changes the actual class of STREAM depending on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((external-format-name (external-format-name external-format))
          (external-format-cr (not (eq (external-format-eol-style external-format) :lf))))
      (change-class stream
                    (cond ((ascii-name-p external-format-name)
                           (if external-format-cr
                             'flexi-cr-ascii-input-stream
                             'flexi-ascii-input-stream))
                          ((eq external-format-name :iso-8859-1)
                           (if external-format-cr
                             'flexi-cr-latin-1-input-stream
                             'flexi-latin-1-input-stream))
                          ((or (koi8-r-name-p external-format-name)
                               (iso-8859-name-p external-format-name)
                               (code-page-name-p external-format-name))
                           (if external-format-cr
                             'flexi-cr-8-bit-input-stream
                             'flexi-8-bit-input-stream))
                          (t (case external-format-name
                               (:utf-8 (if external-format-cr
                                         'flexi-cr-utf-8-input-stream
                                         'flexi-utf-8-input-stream))
                               (:utf-16 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-cr-utf-16-le-input-stream
                                            'flexi-cr-utf-16-be-input-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-utf-16-le-input-stream
                                            'flexi-utf-16-be-input-stream)))
                               (:utf-32 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-cr-utf-32-le-input-stream
                                            'flexi-cr-utf-32-be-input-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-utf-32-le-input-stream
                                            'flexi-utf-32-be-input-stream))))))))))

(defmethod set-class ((stream flexi-output-stream))
  "Changes the actual class of STREAM depending on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((external-format-name (external-format-name external-format))
          (external-format-cr (not (eq (external-format-eol-style external-format) :lf))))
      (change-class stream
                    (cond ((ascii-name-p external-format-name)
                           (if external-format-cr
                             'flexi-cr-ascii-output-stream
                             'flexi-ascii-output-stream))
                          ((eq external-format-name :iso-8859-1)
                           (if external-format-cr
                             'flexi-cr-latin-1-output-stream
                             'flexi-latin-1-output-stream))
                          ((or (koi8-r-name-p external-format-name)
                               (iso-8859-name-p external-format-name)
                               (code-page-name-p external-format-name))
                           (if external-format-cr
                             'flexi-cr-8-bit-output-stream
                             'flexi-8-bit-output-stream))
                          (t (case external-format-name
                               (:utf-8 (if external-format-cr
                                         'flexi-cr-utf-8-output-stream
                                         'flexi-utf-8-output-stream))
                               (:utf-16 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-cr-utf-16-le-output-stream
                                            'flexi-cr-utf-16-be-output-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-utf-16-le-output-stream
                                            'flexi-utf-16-be-output-stream)))
                               (:utf-32 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-cr-utf-32-le-output-stream
                                            'flexi-cr-utf-32-be-output-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-utf-32-le-output-stream
                                            'flexi-utf-32-be-output-stream))))))))))  

(defmethod set-class ((stream flexi-io-stream))
  "Changes the actual class of STREAM depending on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((external-format-name (external-format-name external-format))
          (external-format-cr (not (eq (external-format-eol-style external-format) :lf))))
      (change-class stream
                    (cond ((ascii-name-p external-format-name)
                           (if external-format-cr
                             'flexi-cr-ascii-io-stream
                             'flexi-ascii-io-stream))
                          ((eq external-format-name :iso-8859-1)
                           (if external-format-cr
                             'flexi-cr-latin-1-io-stream
                             'flexi-latin-1-io-stream))
                          ((or (koi8-r-name-p external-format-name)
                               (iso-8859-name-p external-format-name)
                               (code-page-name-p external-format-name))
                           (if external-format-cr
                             'flexi-cr-8-bit-io-stream
                             'flexi-8-bit-io-stream))
                          (t (case external-format-name
                               (:utf-8 (if external-format-cr
                                         'flexi-cr-utf-8-io-stream
                                         'flexi-utf-8-io-stream))
                               (:utf-16 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-cr-utf-16-le-io-stream
                                            'flexi-cr-utf-16-be-io-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-utf-16-le-io-stream
                                            'flexi-utf-16-be-io-stream)))
                               (:utf-32 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-cr-utf-32-le-io-stream
                                            'flexi-cr-utf-32-be-io-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-utf-32-le-io-stream
                                            'flexi-utf-32-be-io-stream))))))))))

(defmethod set-class :after ((stream flexi-stream))
  "After we've changed the actual class of a flexi stream we may
have to set the contents of the 8-bit enconding slots as well."
  (declare (optimize speed))
  (set-encoding-table stream)
  (set-encoding-hash stream))

(defgeneric set-encoding-table (stream)
  (:method (stream))
  (:documentation "Sets the value of the ENCODING-TABLE slot of
STREAM if necessary.  The default method does nothing."))

(defgeneric set-encoding-hash (stream)
  (:method (stream))
  (:documentation "Sets the value of the ENCODING-HASH slot of
STREAM if necessary.  The default method does nothing."))

(defmethod set-encoding-table ((stream flexi-8-bit-input-stream))
  "Sets the value of the ENCODING-TABLE slot of STREAM depending
on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format)
                   (encoding-table flexi-stream-encoding-table))
      stream
    (let ((external-format-name (external-format-name external-format)))
      (setq encoding-table
            (cond ((ascii-name-p external-format-name) +ascii-table+)
                  ((koi8-r-name-p external-format-name) +koi8-r-table+)
                  ((iso-8859-name-p external-format-name)
                   (cdr (assoc external-format-name +iso-8859-tables+ :test #'eq)))
                  ((code-page-name-p external-format-name)
                   (cdr (assoc (external-format-id external-format) +code-page-tables+))))))))

(defmethod set-encoding-hash ((stream flexi-8-bit-output-stream))
  "Sets the value of the ENCODING-HASH slot of STREAM depending
on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format)
                   (encoding-hash flexi-stream-encoding-hash))
      stream
    (let ((external-format-name (external-format-name external-format)))
      (setq encoding-hash
            (cond ((ascii-name-p external-format-name) +ascii-hash+)
                  ((koi8-r-name-p external-format-name) +koi8-r-hash+)
                  ((iso-8859-name-p external-format-name)
                   (cdr (assoc external-format-name +iso-8859-hashes+ :test #'eq)))
                  ((code-page-name-p external-format-name)
                   (cdr (assoc (external-format-id external-format) +code-page-hashes+))))))))


#+:cmu
(defmethod input-stream-p ((stream flexi-io-stream))
  "Explicitly states whether this is an input stream."
  (declare (optimize speed))
  t)

#+:cmu
(defmethod output-stream-p ((stream flexi-io-stream))
  "Explicitly states whether this is an output stream."
  (declare (optimize speed))
  t)

(defun make-flexi-stream (stream &rest args
                                 &key (external-format (make-external-format :iso-8859-1))
                                      element-type column position bound)
  "Creates and returns a new flexi stream.  STREAM must be an open
binary or `bivalent' stream, i.e. it must be capable of
reading/writing octets with READ-SEQUENCE and/or WRITE-SEQUENCE.  The
resulting flexi stream is an input stream if and only if STREAM is an
input stream.  Likewise, it's an output stream if and only if STREAM
is an output stream.  The default for ELEMENT-TYPE is LW:SIMPLE-CHAR
on LispWorks and CHARACTER on other Lisps.  EXTERNAL-FORMAT must be an
EXTERNAL-FORMAT object or a symbol or a list denoting such an object.
COLUMN is the initial column of the stream which is either a
non-negative integer or NIL.  The COLUMN argument must only be used
for output streams.  POSITION \(only used for input streams) should be
an integer and it denotes the position the stream is in - it will be
increased by one for each octet read.  BOUND \(only used for input
streams) should be NIL or an integer.  If BOUND is not NIL and
POSITION has gone beyond BOUND, then the stream will behave as if no
more input is available."
  ;; these arguments are ignored - they are only there to provide a
  ;; meaningful parameter list for IDEs
  (declare (ignore element-type column position bound))
  (unless (and (streamp stream)
               (open-stream-p stream))
    (error "~S should have been an open stream." stream))
  (apply #'make-instance
         ;; actual type depends on STREAM
         (cond ((and (input-stream-p stream)
                     (output-stream-p stream))
                'flexi-io-stream)
               ((input-stream-p stream)
                'flexi-input-stream)
               ((output-stream-p stream)
                'flexi-output-stream))
         :stream stream
         :flexi-stream-external-format external-format
         (sans args :external-format)))
