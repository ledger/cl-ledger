;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/external-format.lisp,v 1.11 2007/01/01 23:46:49 edi Exp $

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

(defclass external-format ()
  ((name :initarg :name
         :reader external-format-name
         :documentation "The name of the external format - a
keyword.")
   (id :initarg :id
       :initform nil
       :reader external-format-id
       :documentation "If the external format denotes a Windows
code page this ID specifies which one to use.  Otherwise the
value is ignored \(and usually NIL).")
   (little-endian :initarg :little-endian
                  :initform *default-little-endian*
                  :reader external-format-little-endian
                  :documentation "Whether multi-octet values are
read and written with the least significant octet first.  For
8-bit encodings like :ISO-8859-1 this value is ignored.")
   (eol-style :initarg :eol-style
              :reader external-format-eol-style
              :documentation "The character\(s) to or from which
a #\Newline will be translated - one of the keywords :CR, :LF,
or :CRLF."))
  (:documentation "EXTERNAL-FORMAT objects are used to denote
encodings for flexi streams."))

(defmethod make-load-form ((thing external-format) &optional environment)
  "Defines a way to reconstruct external formats.  Needed for OpenMCL."
  (make-load-form-saving-slots thing :environment environment))

(defun make-external-format% (name &key (little-endian *default-little-endian*)
                                        id eol-style)
  "Used internally by MAKE-EXTERNAL-FORMAT."
  (let* ((real-name (normalize-external-format-name name))
         (initargs
          (cond ((or (iso-8859-name-p real-name)
		     (koi8-r-name-p real-name)
                     (ascii-name-p real-name))
                 (list :eol-style (or eol-style *default-eol-style*)))
                ((code-page-name-p real-name)
                 (list :id (or (known-code-page-id-p id)
                               (error "Unknown code page ID ~S" id))
                       ;; default EOL style for Windows code pages is :CRLF
                       :eol-style (or eol-style :crlf)))
                (t (list :eol-style (or eol-style *default-eol-style*)
                         :little-endian little-endian)))))
    (apply #'make-instance 'external-format
           :name real-name
           initargs)))

(defun make-external-format (name &rest args
                                  &key (little-endian *default-little-endian*)
                                       id eol-style)
  "Creates and returns an external format object as specified.
NAME is a keyword like :LATIN1 or :UTF-8, LITTLE-ENDIAN specifies
the `endianess' of the external format and is ignored for 8-bit
encodings, EOL-STYLE is one of the keywords :CR, :LF, or :CRLF
which denote the end-of-line character \(sequence), ID is the ID
of a Windows code page \(and ignored for other encodings)."
  (declare (ignore id little-endian))
  (let ((shortcut-args (cdr (assoc name +shortcut-map+))))
    (cond (shortcut-args
           (apply #'make-external-format%
                  (append shortcut-args
                          `(:eol-style ,eol-style))))
          (t (apply #'make-external-format% name args)))))
  
(defun external-format-equal (ef1 ef2)
  "Checks whether two EXTERNAL-FORMAT objects denote the same
encoding."
  (let* ((name1 (external-format-name ef1))
         (code-page-name-p (code-page-name-p name1)))
    ;; they must habe the same canonical name
    (and (eq name1
             (external-format-name ef2))
         ;; if both are code pages the IDs must be the same
         (or (not code-page-name-p)
             (eql (external-format-id ef1)
                  (external-format-id ef2)))
         ;; for non-8-bit encodings the endianess must be the same
         (or code-page-name-p
             (ascii-name-p name1)
	     (koi8-r-name-p name1)
             (iso-8859-name-p name1)
             (eq name1 :utf-8)
             (eq (not (external-format-little-endian ef1))
                 (not (external-format-little-endian ef2))))
         ;; the EOL style must also be the same
         (eq (external-format-eol-style ef1)
             (external-format-eol-style ef2)))))

(defun normalize-external-format (external-format)
  "Returns a list which is a `normalized' representation of the
external format EXTERNAL-FORMAT.  Used internally by
PRINT-OBJECT, for example.  Basically, the result is argument
list that can be fed back to MAKE-EXTERNAL-FORMAT to create an
equivalent object."
  (let ((name (external-format-name external-format))
        (eol-style (external-format-eol-style external-format)))
    (cond ((or (ascii-name-p name)
               (koi8-r-name-p name)
               (iso-8859-name-p name)
               (eq name :utf-8))
           (list name :eol-style eol-style))
          ((code-page-name-p name)
           (list name
                 :id (external-format-id external-format)
                 :eol-style eol-style))
          (t (list name
                   :eol-style eol-style
                   :little-endian (external-format-little-endian external-format))))))

(defmethod print-object ((object external-format) stream)
  "How an EXTERNAL-FORMAT object is rendered.  Uses
NORMALIZE-EXTERNAL-FORMAT."
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (normalize-external-format object) stream)))