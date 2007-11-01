;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/input.lisp,v 1.48 2007/09/06 23:19:24 edi Exp $

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

#-:lispworks
(defmethod read-byte* ((flexi-input-stream flexi-input-stream))
  "Reads one byte \(octet) from the underlying stream of
FLEXI-OUTPUT-STREAM \(or from the internal stack if it's not
empty)."
  (declare (optimize speed))
  ;; we're using S instead of STREAM here because of an
  ;; issue with SBCL:
  ;; <http://article.gmane.org/gmane.lisp.steel-bank.general/1386>
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (s flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from read-byte* nil))
    (incf position)
    (or (pop octet-stack)
        (read-byte s nil nil)
        (progn (decf position) nil))))

#+:lispworks
(defmethod read-byte* ((flexi-input-stream flexi-input-stream))
  "Reads one byte \(octet) from the underlying stream of
FLEXI-OUTPUT-STREAM \(or from the internal stack if it's not
empty)."
  (declare (optimize speed))
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from read-byte* nil))
    (incf position)
    (or (pop octet-stack)
        ;; we use READ-SEQUENCE because READ-BYTE doesn't work with all
        ;; bivalent streams in LispWorks
        (let* ((buffer (make-array 1 :element-type 'octet))
               (new-position (read-sequence buffer stream)))
          (cond ((zerop new-position)
                 (decf position) nil)
                (t (aref buffer 0)))))))

#+:lispworks
(defmethod read-byte* ((flexi-input-stream flexi-binary-input-stream))
  "Reads one byte \(octet) from the underlying stream of
FLEXI-OUTPUT-STREAM \(or from the internal stack if it's not empty).
Optimized version \(only needed for LispWorks) in case the underlying
stream is binary."
  (declare (optimize speed))
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from read-byte* nil))
    (incf position)
    (or (pop octet-stack)
        (read-byte stream nil nil)
        (progn (decf position) nil))))

(defmethod stream-clear-input ((flexi-input-stream flexi-input-stream))
  "Calls the corresponding method for the underlying input stream
and also clears the value of the OCTET-STACK slot."
  (declare (optimize speed))
  ;; note that we don't reset the POSITION slot
  (with-accessors ((octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (setq octet-stack nil)
    (clear-input stream)))

(defmethod stream-listen ((flexi-input-stream flexi-input-stream))
  "Calls the corresponding method for the underlying input stream
but first checks if \(old) input is available in the OCTET-STACK
slot."
  (declare (optimize speed))
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (when (and bound
               (>= position bound))
      (return-from stream-listen nil))
    (or octet-stack (listen stream))))

(defmethod stream-read-byte ((stream flexi-input-stream))
  "Reads one byte \(octet) from the underlying stream."
  (declare (optimize speed))
  ;; set LAST-CHAR-CODE slot to NIL because we can't UNREAD-CHAR after
  ;; this operation
  (with-accessors ((last-char-code flexi-stream-last-char-code)
                   (last-octet flexi-stream-last-octet))
      stream
    (setq last-char-code nil)
    (let ((octet (read-byte* stream)))
      (setq last-octet octet)
      (or octet :eof))))

(defmethod unread-char% (char-code (flexi-input-stream flexi-input-stream))
  "Used internally to put a character denoted by the character code
CHAR-CODE which was already read back on the stream.  Uses the
OCTET-STACK slot and decrements the POSITION slot accordingly."
  (declare (optimize speed) (inline translate-char))
  (with-accessors ((position flexi-stream-position)
                   (octet-stack flexi-stream-octet-stack)
                   (external-format flexi-stream-external-format))
      flexi-input-stream
    (declare (integer position))
    (let ((octets-read (translate-char char-code external-format)))
      (decf position (length octets-read))
      (setq octet-stack (append octets-read octet-stack)))))

(defmacro define-char-reader ((stream-var stream-class) &body body)
  "Helper macro to define methods for STREAM-READ-CHAR.  Defines a
method for the class STREAM-CLASS using the variable STREAM-VAR and
the code body BODY wrapped with some standard code common to all
methods defined here.  The return value of BODY is a character code.
In case of encoding problems, BODY must return the value returned by
\(RECOVER-FROM-ENCODING-ERROR ...)."
  (with-unique-names (char-code body-fn)
    `(defmethod stream-read-char ((,stream-var ,stream-class))
       "This method was generated with the DEFINE-CHAR-READER macro."
       (declare (optimize speed))
       ;; note that we do nothing for the :LF EOL style because we
       ;; assume that #\Newline is the same as #\Linefeed in all
       ;; Lisps which will use this library
       (with-accessors ((last-octet flexi-stream-last-octet)
                        (last-char-code flexi-stream-last-char-code))
           ,stream-var
         ;; set LAST-OCTET slot to NIL because we can't UNREAD-BYTE after
         ;; this operation
         (setq last-octet nil)
         (let ((,char-code
                (flet ((,body-fn () ,@body))
                  (declare (inline ,body-fn) (dynamic-extent (function ,body-fn)))
                  (,body-fn))))
           ;; remember this character and the current external format
           ;; for UNREAD-CHAR
           (setq last-char-code ,char-code)
           (or (code-char ,char-code) ,char-code))))))

(defun recover-from-encoding-error (flexi-stream format-control &rest format-args)
  "Helper function used by the STREAM-READ-CHAR methods below to deal
with encoding errors.  Checks if *SUBSTITUTION-CHAR* is not NIL and
returns its character code in this case.  Otherwise signals a
FLEXI-STREAM-ENCODING-ERROR as determined by the arguments to this
function and provides a corresponding USE-VALUE restart."
  (when *substitution-char*
    (return-from recover-from-encoding-error (char-code *substitution-char*)))
  (restart-case
      (apply #'signal-encoding-error flexi-stream format-control format-args)
    (use-value (char)
      :report "Specify a character to be used instead."
      :interactive (lambda ()
                     (loop
                      (format *query-io* "Type a character: ")
                      (let ((line (read-line *query-io*)))
                        (when (= 1 (length line))
                          (return (list (char line 0)))))))
      (char-code char))))

(define-char-reader (stream flexi-latin-1-input-stream)
  (or (read-byte* stream)
      (return-from stream-read-char :eof)))

(define-char-reader (stream flexi-ascii-input-stream)
  (let ((octet (or (read-byte* stream)
                   (return-from stream-read-char :eof))))
    (declare (type octet octet))
    (if (> octet 127)
      (recover-from-encoding-error stream "No character which corresponds to octet #x~X." octet)
      octet)))

(define-char-reader (stream flexi-8-bit-input-stream)
  (with-accessors ((encoding-table flexi-stream-encoding-table))
      stream
    (let* ((octet (or (read-byte* stream)
                      (return-from stream-read-char :eof)))
           (char-code (aref (the (simple-array * *) encoding-table) octet)))
      (declare (type octet octet))
      (if (or (null char-code)
	      (= char-code 65533))
        (recover-from-encoding-error stream "No character which corresponds to octet #x~X." octet)
        char-code))))

(define-char-reader (stream flexi-utf-8-input-stream)
  (block body
    (let (first-octet-seen)
      (flet ((read-next-byte ()
               (prog1
                   (or (read-byte* stream)
                       (cond (first-octet-seen
			      (return-from body
				(recover-from-encoding-error stream
                                                             "End of file while in UTF-8 sequence.")))
                             (t (return-from stream-read-char :eof))))
                 (setq first-octet-seen t))))
        (declare (inline read-next-byte) (dynamic-extent (function read-next-byte)))
        (let ((octet (read-next-byte)))
          (declare (type octet octet))
          (multiple-value-bind (start count)
              (cond ((zerop (logand octet #b10000000))
                     (values octet 0))
                    ((= #b11000000 (logand octet #b11100000))
                     (values (logand octet #b00011111) 1))
                    ((= #b11100000 (logand octet #b11110000))
                     (values (logand octet #b00001111) 2))
                    ((= #b11110000 (logand octet #b11111000))
                     (values (logand octet #b00000111) 3))
                    ((= #b11111000 (logand octet #b11111100))
                     (values (logand octet #b00000011) 4))
                    ((= #b11111100 (logand octet #b11111110))
                     (values (logand octet #b00000001) 5))
		    (t (return-from body
			 (recover-from-encoding-error stream 
						      "Unexpected value #x~X at start of UTF-8 sequence."
						      octet))))
            ;; note that we currently don't check for "overlong"
            ;; sequences or other illegal values
            (loop for result of-type (unsigned-byte 32)
                  = start then (+ (ash result 6)
                                  (logand octet #b111111))
                  repeat count
                  for octet of-type octet = (read-next-byte)
                  unless (= #b10000000 (logand octet #b11000000))
		  do (return-from body
		       (recover-from-encoding-error stream
                                                    "Unexpected value #x~X in UTF-8 sequence." octet))
		  finally (return result))))))))

(define-char-reader (stream flexi-utf-16-le-input-stream)
  (block body
    (let (first-octet-seen)
      (labels ((read-next-byte ()
                 (prog1
                     (or (read-byte* stream)
                         (cond (first-octet-seen
				(return-from body
				  (recover-from-encoding-error stream
                                                               "End of file while in UTF-16 sequence.")))
                               (t (return-from stream-read-char :eof))))
                   (setq first-octet-seen t)))
               (read-next-word ()
                 (+ (the octet (read-next-byte))
                    (ash (the octet (read-next-byte)) 8))))
        (declare (inline read-next-byte read-next-word)
                 (dynamic-extent (function read-next-byte) (function read-next-word)))
        (let ((word (read-next-word)))
          (cond ((<= #xd800 word #xdfff)
                 (let ((next-word (read-next-word)))
                   (unless (<= #xdc00 next-word #xdfff)
		     (return-from body
		       (recover-from-encoding-error stream "Unexpected UTF-16 word #x~X following #x~X."
						    next-word word)))
                   (+ (ash (logand #b1111111111 word) 10)
                      (logand #b1111111111 next-word)
                      #x10000)))
		(t word)))))))

(define-char-reader (stream flexi-utf-16-be-input-stream)
  (block body
    (let (first-octet-seen)
      (labels ((read-next-byte ()
                 (prog1
                     (or (read-byte* stream)
                         (cond (first-octet-seen
				(return-from body
				  (recover-from-encoding-error stream
                                                               "End of file while in UTF-16 sequence.")))
                               (t (return-from stream-read-char :eof))))
                   (setq first-octet-seen t)))
               (read-next-word ()
                 (+ (ash (the octet (read-next-byte)) 8)
                    (the octet (read-next-byte)))))
        (let ((word (read-next-word)))
          (cond ((<= #xd800 word #xdfff)
                 (let ((next-word (read-next-word)))
                   (unless (<= #xdc00 next-word #xdfff)
		     (return-from body
		       (recover-from-encoding-error stream "Unexpected UTF-16 word #x~X following #x~X."
						    next-word word)))
                   (+ (ash (logand #b1111111111 word) 10)
                      (logand #b1111111111 next-word)
                      #x10000)))
		(t word)))))))

(define-char-reader (stream flexi-utf-32-le-input-stream)
  (block body
    (let (first-octet-seen)
      (flet ((read-next-byte ()
               (prog1
                   (or (read-byte* stream)
                       (cond (first-octet-seen
			      (return-from body
				(recover-from-encoding-error stream
                                                             "End of file while in UTF-32 sequence.")))
                             (t (return-from stream-read-char :eof))))
                 (setq first-octet-seen t))))
        (declare (inline read-next-byte) (dynamic-extent (function read-next-byte)))
        (loop for count from 0 to 24 by 8
              for octet of-type octet = (read-next-byte)
	      sum (ash octet count))))))

(define-char-reader (stream flexi-utf-32-be-input-stream)
  (block body
    (let (first-octet-seen)
      (flet ((read-next-byte ()
               (prog1
                   (or (read-byte* stream)
                       (cond (first-octet-seen
			      (return-from body
				(recover-from-encoding-error stream
                                                             "End of file while in UTF-32 sequence.")))
                             (t (return-from stream-read-char :eof))))
                 (setq first-octet-seen t))))
        (declare (inline read-next-byte) (dynamic-extent (function read-next-byte)))
        (loop for count from 24 downto 0 by 8
              for octet of-type octet = (read-next-byte)
	      sum (ash octet count))))))

(defmethod stream-read-char ((stream flexi-cr-mixin))
  "The `base' method for all streams which need end-of-line
conversion.  Uses CALL-NEXT-METHOD to do the actual work of
reading one or more characters from the stream."
  (declare (optimize speed))
  (let ((char (call-next-method)))
    (when (eq char :eof)
      (return-from stream-read-char :eof))
    (with-accessors ((external-format flexi-stream-external-format)
                     (last-char-code flexi-stream-last-char-code))
        stream
      (when (eql char #\Return)
        (case (external-format-eol-style external-format)
          (:cr (setq char #\Newline
                     last-char-code #.(char-code #\Newline)))
          ;; in the case :CRLF we have to look ahead one character
          (:crlf (let ((next-char (call-next-method)))
                   (case next-char
                     (#\Linefeed
                      (setq char #\Newline
                            last-char-code #.(char-code #\Newline)))
                     (:eof)
                     ;; if the character we peeked at wasn't a
                     ;; linefeed character we push its
                     ;; constituents back onto our internal
                     ;; octet stack
                     (otherwise (unread-char% (char-code next-char) stream)))))))
      char)))

(defmethod stream-read-char-no-hang ((stream flexi-input-stream))
  "Reads one character if the underlying stream has at least one
octet available."
  (declare (optimize speed))
  ;; note that this may block for non-8-bit encodings - I think
  ;; there's no easy way to handle this correctly
  (and (stream-listen stream)
       (stream-read-char stream)))

(defmethod stream-read-sequence ((flexi-input-stream flexi-input-stream) sequence start end &key)
  "Reads enough input from STREAM to fill SEQUENCE from START to END.
If SEQUENCE is an array which can store octets we use READ-SEQUENCE to
fill it in one fell swoop, otherwise we iterate using
STREAM-READ-CHAR."
  (declare (optimize speed)
           (type (integer 0 *) start end))
  (with-accessors ((last-char-code flexi-stream-last-char-code)
                   (last-octet flexi-stream-last-octet)
                   (stream flexi-stream-stream)
                   (position flexi-stream-position)
                   (octet-stack flexi-stream-octet-stack))
      flexi-input-stream
    (declare (integer position))
    (cond ((and (arrayp sequence)
                (subtypep 'octet (array-element-type sequence)))
           (setf last-char-code nil)
           (let ((cursor start))
             (loop with stack = octet-stack
                   for continuep = (< cursor end)
                   for octet = (and continuep (pop stack))
                   while octet
                   do (setf (aref sequence cursor) (the octet octet))
                   (incf cursor))
             (let ((index
                    (read-sequence sequence stream :start cursor :end end)))
               (incf position (- index start))
               (when (> index start)
                 (setq last-octet (aref sequence (1- index))))
               index)))
          (t
           (loop for index from start below end
                 for element = (stream-read-char flexi-input-stream)
                 until (eq element :eof)
                 do (setf (elt sequence index) element)
                 finally (return index))))))

(defmethod stream-unread-char ((stream flexi-input-stream) char)
  "Implements UNREAD-CHAR for streams of type FLEXI-INPUT-STREAM.
Makes sure CHAR will only be unread if it was the last character
read and if it was read with the same encoding that's currently
being used by the stream."
  (declare (optimize speed))
  (with-accessors ((last-char-code flexi-stream-last-char-code))
      stream
    (unless last-char-code
      (error 'flexi-stream-simple-error
             :format-control "No character to unread from this stream \(or external format has changed or last reading operation was binary)."))
    (unless (= (char-code char) last-char-code)
      (error 'flexi-stream-simple-error
             :format-control "Last character read was different from ~S."
             :format-arguments (list char)))
    (unread-char% last-char-code stream)
    (setq last-char-code nil)
    nil))

(defmethod unread-byte (byte (flexi-input-stream flexi-input-stream))
  "Similar to UNREAD-CHAR in that it `unreads' the last octet from
STREAM.  Note that you can only call UNREAD-BYTE after a corresponding
READ-BYTE."
  (declare (optimize speed))
  (with-accessors ((last-octet flexi-stream-last-octet)
                   (octet-stack flexi-stream-octet-stack)
                   (position flexi-stream-position))
      flexi-input-stream
    (unless last-octet
      (error 'flexi-stream-simple-error
             :format-control "No byte to unread from this stream \(or last reading operation read a character)."))
    (unless (= byte last-octet)
      (error 'flexi-stream-simple-error
             :format-control "Last byte read was different from #x~X."
             :format-arguments (list byte)))
    (setq last-octet nil)
    (decf (the integer position))
    (push byte octet-stack)
    nil))
    
(defmethod peek-byte ((flexi-input-stream flexi-input-stream)
                      &optional peek-type (eof-error-p t) eof-value)
  "PEEK-BYTE is like PEEK-CHAR, i.e. it returns an octet from
FLEXI-INPUT-STREAM without actually removing it.  If PEEK-TYPE is NIL
the next octet is returned, if PEEK-TYPE is T, the next octet which is
not 0 is returned, if PEEK-TYPE is an octet, the next octet which
equals PEEK-TYPE is returned.  EOF-ERROR-P and EOF-VALUE are
interpreted as usual."
  (declare (optimize speed))
  (loop for octet = (read-byte flexi-input-stream eof-error-p eof-value)
        until (cond ((null peek-type))
                    ((eql octet eof-value))
                    ((eq peek-type t)
                     (plusp octet))
                    (t (= octet peek-type)))
        finally (unless (eql octet eof-value)
                  (unread-byte octet flexi-input-stream))
                (return octet)))