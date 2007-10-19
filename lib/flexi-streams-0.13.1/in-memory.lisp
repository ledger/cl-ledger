;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/in-memory.lisp,v 1.25 2007/01/12 00:08:15 edi Exp $

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

(defclass in-memory-stream (trivial-gray-stream-mixin)
  ((transformer :initarg :transformer
                :accessor in-memory-stream-transformer
                :documentation "A function used to transform the
written/read octet to the value stored/retrieved in/from the
underlying vector.")   
   #+:cmu
   (open-p :initform t
           :accessor in-memory-stream-open-p
           :documentation "For CMUCL we have to keep track of this
manually."))
  (:documentation "An IN-MEMORY-STREAM is a binary stream that reads
octets from or writes octets to a sequence in RAM."))

(defclass in-memory-input-stream (in-memory-stream fundamental-binary-input-stream)
  ()
  (:documentation "An IN-MEMORY-INPUT-STREAM is a binary stream that
reads octets from a sequence in RAM."))

#+:cmu
(defmethod output-stream-p ((stream in-memory-input-stream))
  "Explicitly states whether this is an output stream."
  (declare (optimize speed))
  nil)

(defclass in-memory-output-stream (in-memory-stream fundamental-binary-output-stream)
  ()
  (:documentation "An IN-MEMORY-OUTPUT-STREAM is a binary stream that
writes octets to a sequence in RAM."))

#+:cmu
(defmethod input-stream-p ((stream in-memory-output-stream))
  "Explicitly states whether this is an input stream."
  (declare (optimize speed))
  nil)

(defclass list-stream ()
  ((list :initarg :list
         :accessor list-stream-list
         :documentation "The underlying list of the stream."))
  (:documentation "A LIST-STREAM is a mixin for IN-MEMORY streams
where the underlying sequence is a list."))

(defclass vector-stream ()
  ((vector :initarg :vector
           :accessor vector-stream-vector
           :documentation "The underlying vector of the stream which
\(for output) must always be adjustable and have a fill pointer."))
  (:documentation "A VECTOR-STREAM is a mixin for IN-MEMORY streams
where the underlying sequence is a vector."))

(defclass list-input-stream (list-stream in-memory-input-stream)
  ()
  (:documentation "A binary input stream that gets its data from an
associated list of octets."))

(defclass vector-input-stream (vector-stream in-memory-input-stream)
  ((index :initarg :index
          :accessor vector-stream-index
          :type (integer 0 #.array-dimension-limit)
          :documentation "An index into the underlying vector denoting
the current position.")
   (end :initarg :end
        :accessor vector-stream-end
        :type (integer 0 #.array-dimension-limit)
        :documentation "An index into the underlying vector denoting
the end of the available data."))
  (:documentation "A binary input stream that gets its data from an
associated vector of octets."))

(defclass vector-output-stream (vector-stream in-memory-output-stream)
  ()
  (:documentation "A binary output stream that writes its data to an
associated vector."))

(define-condition in-memory-stream-error (stream-error)
  ()
  (:documentation "Superclass for all errors related to
IN-MEMORY streams."))

(define-condition in-memory-stream-closed-error (in-memory-stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is closed."
                     (stream-error-stream condition))))
  (:documentation "An error that is signaled when someone is trying
to read from or write to a closed IN-MEMORY stream."))

#+:cmu
(defmethod open-stream-p ((stream in-memory-stream))
  "Returns a true value if STREAM is open.  See ANSI standard."
  (declare (optimize speed))
  (in-memory-stream-open-p stream))

#+:cmu
(defmethod close ((stream in-memory-stream) &key abort)
  "Closes the stream STREAM.  See ANSI standard."
  (declare (ignore abort)
           (optimize speed))
  (prog1
      (in-memory-stream-open-p stream)
    (setf (in-memory-stream-open-p stream) nil)))

(defmethod check-if-open ((stream in-memory-stream))
  "Checks if STREAM is open and signals an error otherwise."
  (declare (optimize speed))
  (unless (open-stream-p stream)
    (error 'in-memory-stream-closed-error
           :stream stream)))

(defmethod stream-element-type ((stream in-memory-stream))
  "The element type is always OCTET by definition."
  (declare (optimize speed))
  'octet)

(defmethod transform-octet ((stream in-memory-stream) octet)
  "Applies the transformer of STREAM to octet and returns the result."
  (funcall (or (in-memory-stream-transformer stream)
               #'identity) octet))

(defmethod stream-read-byte ((stream list-input-stream))
  "Reads one byte by simply popping it off of the top of the list."
  (declare (optimize speed))
  (check-if-open stream)
  (transform-octet stream (or (pop (list-stream-list stream))
                              (return-from stream-read-byte :eof))))

(defmethod stream-listen ((stream list-input-stream))
  "Checks whether list is not empty."
  (declare (optimize speed))
  (check-if-open stream)
  (list-stream-list stream))

(defmethod stream-read-sequence ((stream list-input-stream) sequence start end &key)
  "Repeatedly pops elements from the list until it's empty."
  (declare (optimize speed) (type (integer 0 *) start end))
  (loop for index from start below end
        while (list-stream-list stream)
        do (setf (elt sequence index)
                 (pop (list-stream-list stream)))
        finally (return index)))

(defmethod stream-read-byte ((stream vector-input-stream))
  "Reads one byte and increments INDEX pointer unless we're beyond
END pointer."
  (declare (optimize speed))
  (check-if-open stream)
  (let ((index (vector-stream-index stream)))
    (cond ((< index (vector-stream-end stream))
           (incf (vector-stream-index stream))
           (transform-octet stream (aref (vector-stream-vector stream) index)))
          (t :eof))))

(defmethod stream-listen ((stream vector-input-stream))
  "Checking whether INDEX is beyond END."
  (declare (optimize speed))
  (check-if-open stream)
  (< (vector-stream-index stream) (vector-stream-end stream)))
  
(defmethod stream-read-sequence ((stream vector-input-stream) sequence start end &key)
  "Traverses both sequences in parallel until the end of one of them
is reached."
  (declare (optimize speed) (type (integer 0 *) start end))
  (loop with vector-end of-type (integer 0 #.array-dimension-limit) = (vector-stream-end stream)
        with vector = (vector-stream-vector stream)
        for index from start below end
        for vector-index of-type (integer 0 #.array-dimension-limit) = (vector-stream-index stream)
        while (< vector-index vector-end)
        do (setf (elt sequence index)
                 (aref vector vector-index))
           (incf (vector-stream-index stream))
        finally (return index)))

(defmethod stream-write-byte ((stream vector-output-stream) byte)
  "Writes a byte \(octet) by extending the underlying vector."
  (declare (optimize speed))
  (check-if-open stream)
  (vector-push-extend (transform-octet stream byte)
                      (vector-stream-vector stream)))

(defmethod stream-write-sequence ((stream vector-output-stream) sequence start end &key)
  "Just calls VECTOR-PUSH-EXTEND repeatedly."
  (declare (optimize speed) (type (integer 0 *) start end))
  (loop with vector = (vector-stream-vector stream)
        for index from start below end
        do (vector-push-extend (elt sequence index) vector))
  sequence)

(defmethod stream-file-position ((stream vector-input-stream))
  "Simply returns the index into the underlying vector."
  (declare (optimize speed))
  (vector-stream-index stream))

(defmethod (setf stream-file-position) (position-spec (stream vector-input-stream))
  "Sets the index into the underlying vector if POSITION-SPEC is acceptable."
  (declare (optimize speed))
  (setf (vector-stream-index stream)
        (case position-spec
          (:start 0)
          (:end (vector-stream-end stream))
          (otherwise
           (unless (integerp position-spec)
             (error 'flexi-stream-position-spec-error
                    :format-control "Unknown file position designator: ~S."
                    :format-arguments (list position-spec)
                    :position-spec position-spec))
           (unless (<= 0 position-spec (vector-stream-end stream))
             (error 'flexi-stream-position-spec-error
                    :format-control "File position designator ~S is out of bounds."
                    :format-arguments (list position-spec)
                    :position-spec position-spec))
           position-spec)))
  position-spec)

(defmethod stream-file-position ((stream vector-output-stream))
  "Simply returns the fill pointer of the underlying vector."
  (declare (optimize speed))
  (fill-pointer (vector-stream-vector stream)))

(defmethod (setf stream-file-position) (position-spec (stream vector-output-stream))
  "Sets the fill pointer underlying vector if POSITION-SPEC is
acceptable.  Adjusts the vector if necessary."
  (declare (optimize speed))
  (let* ((vector (vector-stream-vector stream))
         (total-size (array-total-size vector))
         (new-fill-pointer
          (case position-spec
            (:start 0)
            (:end
             (warn "File position designator :END doesn't really make sense for an output stream.")
             total-size)
            (otherwise
             (unless (integerp position-spec)
               (error 'flexi-stream-position-spec-error
                      :format-control "Unknown file position designator: ~S."
                      :format-arguments (list position-spec)
                      :position-spec position-spec))
             (unless (<= 0 position-spec array-total-size-limit)
               (error 'flexi-stream-position-spec-error
                      :format-control "File position designator ~S is out of bounds."
                      :format-arguments (list position-spec)
                      :position-spec position-spec))
             position-spec))))
    (when (> new-fill-pointer total-size)
      (adjust-array vector new-fill-pointer))
    (setf (fill-pointer vector) new-fill-pointer)
    position-spec))

(defmethod make-in-memory-input-stream ((vector vector) &key (start 0)
                                                             (end (length vector))
                                                             transformer)
  "Returns a binary input stream which will supply, in order, the
octets in the subsequence of VECTOR bounded by START and END.
Each octet returned will be transformed in turn by the optional
TRANSFORMER function."
  (declare (optimize speed))
  (make-instance 'vector-input-stream
                 :vector vector
                 :index start
                 :end end
                 :transformer transformer))

(defmethod make-in-memory-input-stream ((list list) &key (start 0)
                                                         (end (length list))
                                                         transformer)
  "Returns a binary input stream which will supply, in order, the
octets in the subsequence of LIST bounded by START and END.  Each
octet returned will be transformed in turn by the optional
TRANSFORMER function."
  (declare (optimize speed))
  (make-instance 'list-input-stream
                 :list (subseq list start end)
                 :transformer transformer))

(defun make-output-vector (&key (element-type 'octet))
  "Creates and returns an array which can be used as the underlying
vector for a VECTOR-OUTPUT-STREAM."
  (declare (optimize speed))
  (make-array 0 :adjustable t
                :fill-pointer 0
                :element-type element-type))

(defun make-in-memory-output-stream (&key (element-type 'octet) transformer)
  "Returns a binary output stream which accepts objects of type
ELEMENT-TYPE \(a subtype of OCTET) and makes available a sequence
that contains the octes that were actually output.  The octets
stored will each be transformed by the optional TRANSFORMER
function."
  (declare (optimize speed))
  (make-instance 'vector-output-stream
                 :vector (make-output-vector :element-type element-type)
                 :transformer transformer))

(defmethod get-output-stream-sequence ((stream in-memory-output-stream) &key as-list)
  "Returns a vector containing, in order, all the octets that have
been output to the IN-MEMORY stream STREAM. This operation clears any
octets on STREAM, so the vector contains only those octets which have
been output since the last call to GET-OUTPUT-STREAM-SEQUENCE or since
the creation of the stream, whichever occurred most recently.  If
AS-LIST is true the return value is coerced to a list."
  (declare (optimize speed))
  (prog1
      (if as-list
        (coerce (vector-stream-vector stream) 'list)
        (vector-stream-vector stream))
    (setf (vector-stream-vector stream)
          (make-output-vector))))

(defmethod output-stream-sequence-length ((stream in-memory-output-stream))
  "Returns the current length of the underlying vector of the
IN-MEMORY output stream STREAM."
  (declare (optimize speed))
  (length (the (simple-array * (*)) (vector-stream-vector stream))))

(defmacro with-input-from-sequence ((var sequence &key start end transformer) 
                                    &body body)
  "Creates an IN-MEMORY input stream from SEQUENCE using the
parameters START and END, binds VAR to this stream and then
executes the code in BODY.  A function TRANSFORMER may optionally
be specified to transform the returned octets.  The stream is
automatically closed on exit from WITH-INPUT-FROM-SEQUENCE, no
matter whether the exit is normal or abnormal.  The return value
of this macro is the return value of BODY."
  (with-rebinding (sequence)
    `(let (,var)
       (unwind-protect
           (progn
             (setq ,var (make-in-memory-input-stream ,sequence
                                                     :start (or ,start 0)
                                                     :end (or ,end (length ,sequence))
                                                     :transformer ,transformer))
             ,@body)
         (when ,var (close ,var))))))

(defmacro with-output-to-sequence ((var &key as-list (element-type ''octet) transformer)
                                   &body body)
  "Creates an IN-MEMORY output stream, binds VAR to this stream
and then executes the code in BODY.  The stream stores data of
type ELEMENT-TYPE \(a subtype of OCTET) which is \(optionally)
transformed by the function TRANSFORMER prior to storage.  The
stream is automatically closed on exit from
WITH-OUTPUT-TO-SEQUENCE, no matter whether the exit is normal or
abnormal.  The return value of this macro is a vector \(or a list
if AS-LIST is true) containing the octets that were sent to the
stream within BODY."
  `(let (,var)
     (unwind-protect
         (progn
           (setq ,var (make-in-memory-output-stream :element-type ,element-type
                                                    :transformer ,transformer))
           ,@body
           (get-output-stream-sequence ,var :as-list ,as-list))
       (when ,var (close ,var)))))

(declaim (inline translate-char))
(defun translate-char (char-code external-format)
  "Returns a list of octets which correspond to the
representation of the character with character code CHAR-CODE
when sent to a flexi stream with external format EXTERNAL-FORMAT.
Used internally by UNREAD-CHAR%.  See also STRING-TO-OCTETS."
  (declare (optimize speed))
  (with-output-to-sequence (list :as-list t)
    (let ((stream (make-flexi-stream list :external-format external-format)))
      (write-char (code-char char-code) stream))))