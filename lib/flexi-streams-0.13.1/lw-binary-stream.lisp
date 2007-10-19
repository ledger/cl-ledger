;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/lw-binary-stream.lisp,v 1.10 2007/01/01 23:46:49 edi Exp $

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

(defclass flexi-binary-output-stream (flexi-output-stream)
  ()
  (:documentation "This class is for output streams where the
underlying stream is binary.  It exists solely for the purpose of
optimizing output on LispWorks.  See WRITE-BYTE*."))

(defclass flexi-binary-input-stream (flexi-input-stream)
  ()
  (:documentation "This class is for input streams where the
underlying stream is binary.  It exists solely for the purpose of
optimizing input on LispWorks.  See READ-BYTE*."))

(defclass flexi-binary-io-stream (flexi-binary-input-stream flexi-binary-output-stream flexi-io-stream)
  ()
  (:documentation "This class is for bidirectional streams where the
underlying stream is binary.  It exists solely for the purpose of
optimizing input and output on LispWorks.  See READ-BYTE* and
WRITE-BYTE*."))

(defclass flexi-binary-8-bit-input-stream (flexi-8-bit-input-stream flexi-binary-input-stream)
  ()
  (:documentation "Like FLEXI-8-BIT-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-8-bit-input-stream (flexi-cr-mixin flexi-binary-8-bit-input-stream)
  ()
  (:documentation "Like FLEXI-CR-8-BIT-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-ascii-input-stream (flexi-ascii-input-stream flexi-binary-8-bit-input-stream)
  ()
  (:documentation "Like FLEXI-ASCII-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-ascii-input-stream (flexi-cr-mixin flexi-binary-ascii-input-stream)
  ()
  (:documentation "Like FLEXI-CR-ASCII-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-latin-1-input-stream (flexi-latin-1-input-stream flexi-binary-8-bit-input-stream)
  ()
  (:documentation "Like FLEXI-LATIN-1-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-latin-1-input-stream (flexi-cr-mixin flexi-binary-latin-1-input-stream)
  ()
  (:documentation "Like FLEXI-CR-LATIN-1-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-32-le-input-stream (flexi-utf-32-le-input-stream flexi-binary-input-stream)
  ()
  (:documentation "Like FLEXI-UTF-32-LE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-32-le-input-stream (flexi-cr-mixin flexi-binary-utf-32-le-input-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-32-LE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-32-be-input-stream (flexi-utf-32-be-input-stream flexi-binary-input-stream)
  ()
  (:documentation "Like FLEXI-UTF-32-BE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-32-be-input-stream (flexi-cr-mixin flexi-binary-utf-32-be-input-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-32-BE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-16-le-input-stream (flexi-utf-16-le-input-stream flexi-binary-input-stream)
  ()
  (:documentation "Like FLEXI-UTF-16-LE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-16-le-input-stream (flexi-cr-mixin flexi-binary-utf-16-le-input-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-16-LE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-16-be-input-stream (flexi-utf-16-be-input-stream flexi-binary-input-stream)
  ()
  (:documentation "Like FLEXI-UTF-16-BE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-16-be-input-stream (flexi-cr-mixin flexi-binary-utf-16-be-input-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-16-BE-INPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-8-input-stream (flexi-utf-8-input-stream flexi-binary-input-stream)
  ()
  (:documentation "Like FLEXI-UTF-8-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-8-input-stream (flexi-cr-mixin flexi-binary-utf-8-input-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-8-INPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-8-bit-output-stream (flexi-8-bit-output-stream flexi-binary-output-stream)
  ()
  (:documentation "Like FLEXI-8-BIT-OUTPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-8-bit-output-stream (flexi-cr-mixin flexi-binary-8-bit-output-stream)
  ()
  (:documentation "Like FLEXI-CR-8-BIT-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-ascii-output-stream (flexi-ascii-output-stream flexi-binary-8-bit-output-stream)
  ()
  (:documentation "Like FLEXI-ASCII-OUTPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-ascii-output-stream (flexi-cr-mixin flexi-binary-ascii-output-stream)
  ()
  (:documentation "Like FLEXI-CR-ASCII-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-latin-1-output-stream (flexi-latin-1-output-stream flexi-binary-8-bit-output-stream)
  ()
  (:documentation "Like FLEXI-LATIN-1-OUTPUT-STREAM but optimized
for LispWorks binary streams."))

(defclass flexi-binary-cr-latin-1-output-stream (flexi-cr-mixin flexi-binary-latin-1-output-stream)
  ()
  (:documentation "Like FLEXI-CR-LATIN-1-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-32-le-output-stream (flexi-utf-32-le-output-stream flexi-binary-output-stream)
  ()
  (:documentation "Like FLEXI-UTF-32-LE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-32-le-output-stream (flexi-cr-mixin flexi-binary-utf-32-le-output-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-32-LE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-32-be-output-stream (flexi-utf-32-be-output-stream flexi-binary-output-stream)
  ()
  (:documentation "Like FLEXI-UTF-32-BE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-32-be-output-stream (flexi-cr-mixin flexi-binary-utf-32-be-output-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-32-BE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-16-le-output-stream (flexi-utf-16-le-output-stream flexi-binary-output-stream)
  ()
  (:documentation "Like FLEXI-UTF-16-LE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-16-le-output-stream (flexi-cr-mixin flexi-binary-utf-16-le-output-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-16-LE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-16-be-output-stream (flexi-utf-16-be-output-stream flexi-binary-output-stream)
  ()
  (:documentation "Like FLEXI-UTF-16-BE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-16-be-output-stream (flexi-cr-mixin flexi-binary-utf-16-be-output-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-16-BE-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-8-output-stream (flexi-utf-8-output-stream flexi-binary-output-stream)
  ()
  (:documentation "Like FLEXI-UTF-8-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-8-output-stream (flexi-cr-mixin flexi-binary-utf-8-output-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-8-OUTPUT-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-8-bit-io-stream (flexi-binary-io-stream flexi-8-bit-io-stream)
  ()
  (:documentation "Like FLEXI-8-BIT-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-8-bit-io-stream (flexi-cr-mixin flexi-binary-8-bit-io-stream)
  ()
  (:documentation "Like FLEXI-CR-8-BIT-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-ascii-io-stream (flexi-ascii-io-stream flexi-binary-8-bit-io-stream)
  ()
  (:documentation "Like FLEXI-ASCII-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-ascii-io-stream (flexi-cr-mixin flexi-binary-ascii-io-stream)
  ()
  (:documentation "Like FLEXI-CR-ASCII-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-latin-1-io-stream (flexi-latin-1-io-stream flexi-binary-8-bit-io-stream)
  ()
  (:documentation "Like FLEXI-LATIN-1-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-latin-1-io-stream (flexi-cr-mixin flexi-binary-latin-1-io-stream)
  ()
  (:documentation "Like FLEXI-CR-LATIN-1-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-32-le-io-stream (flexi-utf-32-le-io-stream flexi-binary-io-stream)
  ()
  (:documentation "Like FLEXI-UTF-32-LE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-32-le-io-stream (flexi-cr-mixin flexi-binary-utf-32-le-io-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-32-LE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-32-be-io-stream (flexi-utf-32-be-io-stream flexi-binary-io-stream)
  ()
  (:documentation "Like FLEXI-UTF-32-BE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-32-be-io-stream (flexi-cr-mixin flexi-binary-utf-32-be-io-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-32-BE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-16-le-io-stream (flexi-utf-16-le-io-stream flexi-binary-io-stream)
  ()
  (:documentation "Like FLEXI-UTF-16-LE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-16-le-io-stream (flexi-cr-mixin flexi-binary-utf-16-le-io-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-16-LE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-16-be-io-stream (flexi-utf-16-be-io-stream flexi-binary-io-stream)
  ()
  (:documentation "Like FLEXI-UTF-16-BE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-16-be-io-stream (flexi-cr-mixin flexi-binary-utf-16-be-io-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-16-BE-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-utf-8-io-stream (flexi-utf-8-io-stream flexi-binary-io-stream)
  ()
  (:documentation "Like FLEXI-UTF-8-IO-STREAM but
optimized for LispWorks binary streams."))

(defclass flexi-binary-cr-utf-8-io-stream (flexi-cr-mixin flexi-binary-utf-8-io-stream)
  ()
  (:documentation "Like FLEXI-CR-UTF-8-IO-STREAM but
optimized for LispWorks binary streams."))

(defmethod set-class ((stream flexi-binary-input-stream))
  "Changes the actual class of STREAM depending on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((external-format-name (external-format-name external-format))
          (external-format-cr (not (eq (external-format-eol-style external-format) :lf))))
      (change-class stream
                    (cond ((ascii-name-p external-format-name)
                           (if external-format-cr
                             'flexi-binary-cr-ascii-input-stream
                             'flexi-binary-ascii-input-stream))
                          ((eq external-format-name :iso-8859-1)
                           (if external-format-cr
                             'flexi-binary-cr-latin-1-input-stream
                             'flexi-binary-latin-1-input-stream))
                          ((or (koi8-r-name-p external-format-name)
                               (iso-8859-name-p external-format-name)
                               (code-page-name-p external-format-name))
                           (if external-format-cr
                             'flexi-binary-cr-8-bit-input-stream
                             'flexi-binary-8-bit-input-stream))
                          (t (case external-format-name
                               (:utf-8 (if external-format-cr
                                         'flexi-binary-cr-utf-8-input-stream
                                         'flexi-binary-utf-8-input-stream))
                               (:utf-16 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-cr-utf-16-le-input-stream
                                            'flexi-binary-cr-utf-16-be-input-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-utf-16-le-input-stream
                                            'flexi-binary-utf-16-be-input-stream)))
                               (:utf-32 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-cr-utf-32-le-input-stream
                                            'flexi-binary-cr-utf-32-be-input-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-utf-32-le-input-stream
                                            'flexi-binary-utf-32-be-input-stream))))))))))

(defmethod set-class ((stream flexi-binary-output-stream))
  "Changes the actual class of STREAM depending on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((external-format-name (external-format-name external-format))
          (external-format-cr (not (eq (external-format-eol-style external-format) :lf))))
      (change-class stream
                    (cond ((ascii-name-p external-format-name)
                           (if external-format-cr
                             'flexi-binary-cr-ascii-output-stream
                             'flexi-binary-ascii-output-stream))
                          ((eq external-format-name :iso-8859-1)
                           (if external-format-cr
                             'flexi-binary-cr-latin-1-output-stream
                             'flexi-binary-latin-1-output-stream))
                          ((or (koi8-r-name-p external-format-name)
                               (iso-8859-name-p external-format-name)
                               (code-page-name-p external-format-name))
                           (if external-format-cr
                             'flexi-binary-cr-8-bit-output-stream
                             'flexi-binary-8-bit-output-stream))
                          (t (case external-format-name
                               (:utf-8 (if external-format-cr
                                         'flexi-binary-cr-utf-8-output-stream
                                         'flexi-binary-utf-8-output-stream))
                               (:utf-16 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-cr-utf-16-le-output-stream
                                            'flexi-binary-cr-utf-16-be-output-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-utf-16-le-output-stream
                                            'flexi-binary-utf-16-be-output-stream)))
                               (:utf-32 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-cr-utf-32-le-output-stream
                                            'flexi-binary-cr-utf-32-be-output-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-utf-32-le-output-stream
                                            'flexi-binary-utf-32-be-output-stream))))))))))  

(defmethod set-class ((stream flexi-binary-io-stream))
  "Changes the actual class of STREAM depending on its external format."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((external-format-name (external-format-name external-format))
          (external-format-cr (not (eq (external-format-eol-style external-format) :lf))))
      (change-class stream
                    (cond ((ascii-name-p external-format-name)
                           (if external-format-cr
                             'flexi-binary-cr-ascii-io-stream
                             'flexi-binary-ascii-io-stream))
                          ((eq external-format-name :iso-8859-1)
                           (if external-format-cr
                             'flexi-binary-cr-latin-1-io-stream
                             'flexi-binary-latin-1-io-stream))
                          ((or (koi8-r-name-p external-format-name)
                               (iso-8859-name-p external-format-name)
                               (code-page-name-p external-format-name))
                           (if external-format-cr
                             'flexi-binary-cr-8-bit-io-stream
                             'flexi-binary-8-bit-io-stream))
                          (t (case external-format-name
                               (:utf-8 (if external-format-cr
                                         'flexi-binary-cr-utf-8-io-stream
                                         'flexi-binary-utf-8-io-stream))
                               (:utf-16 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-cr-utf-16-le-io-stream
                                            'flexi-binary-cr-utf-16-be-io-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-utf-16-le-io-stream
                                            'flexi-binary-utf-16-be-io-stream)))
                               (:utf-32 (if external-format-cr
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-cr-utf-32-le-io-stream
                                            'flexi-binary-cr-utf-32-be-io-stream)
                                          (if (external-format-little-endian external-format)
                                            'flexi-binary-utf-32-le-io-stream
                                            'flexi-binary-utf-32-be-io-stream))))))))))
                         

(defmethod initialize-instance :after ((flexi-stream flexi-output-stream) &rest initargs)
  "Might change the class of FLEXI-STREAM for optimization purposes.
Only needed for LispWorks."
  (declare (ignore initargs)
           (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-stream
    (when (subtypep (stream-element-type stream) 'octet)
      (change-class flexi-stream
                    (typecase flexi-stream
                      (flexi-io-stream 'flexi-binary-io-stream)
                      (otherwise 'flexi-binary-output-stream)))
      (set-class flexi-stream))))

(defmethod initialize-instance :after ((flexi-stream flexi-input-stream) &rest initargs)
  "Might change the class of FLEXI-STREAM for optimization purposes.
Only needed for LispWorks."
  (declare (ignore initargs)
           (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-stream
    (when (subtypep (stream-element-type stream) 'octet)
      (change-class flexi-stream
                    (typecase flexi-stream
                      (flexi-io-stream 'flexi-binary-io-stream)
                      (otherwise 'flexi-binary-input-stream)))
      (set-class flexi-stream))))
