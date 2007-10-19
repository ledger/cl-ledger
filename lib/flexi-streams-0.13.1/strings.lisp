;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/strings.lisp,v 1.4 2007/01/01 23:46:49 edi Exp $

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

(defun string-to-octets (string &key (external-format (make-external-format :latin1))
                                     (start 0) end)
  "Converts the Lisp string STRING from START to END to an array of
octets corresponding to the external format EXTERNAL-FORMAT."
  (declare (optimize speed))
  (with-output-to-sequence (out)
    (let ((flexi (make-flexi-stream out :external-format external-format)))
      (write-string string flexi :start start :end end))))

(defun octets-to-string (vector &key (external-format (make-external-format :latin1))
                                     (start 0) (end (length vector)))
  "Converts the Lisp vector VECTOR of octets from START to END to
string using the external format EXTERNAL-FORMAT."
  (declare (optimize speed))
  (with-input-from-sequence (in vector :start start :end end)
    (let ((flexi (make-flexi-stream in :external-format external-format))
          (result (make-array (- end start)
                              :element-type #+:lispworks 'lw:simple-char
                                            #-:lispworks 'character
                              :fill-pointer t)))
      (setf (fill-pointer result)
            (read-sequence result flexi))
      result)))
                              

