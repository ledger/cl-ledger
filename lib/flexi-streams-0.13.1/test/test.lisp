;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/test/test.lisp,v 1.12 2007/03/09 01:14:30 edi Exp $

;;; Copyright (c) 2006-2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :flexi-streams-test)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The pathname of the file (`test.lisp') where this variable was
defined.")

(defvar *tmp-dir*
  (load-time-value
    (merge-pathnames "flexi-streams-test/"
                     #+:allegro (system:temporary-directory)
                     #+:lispworks (pathname (or (lw:environment-variable "TEMP")
                                                (lw:environment-variable "TMP")
                                                #+:win32 "C:/"
                                                #-:win32 "/tmp/"))
                     #-(or :allegro :lispworks) #p"/tmp/"))
  "The pathname of a temporary directory used for testing.")

(defvar *test-files*
  '(("kafka" (:utf8 :latin1 :cp1252))
    ("tilton" (:utf8 :ascii))
    ("hebrew" (:utf8 :latin8))
    ("russian" (:utf8 :koi8r))
    ("unicode_demo" (:utf8 :ucs2 :ucs4)))
  "A list of test files where each entry consists of the name
prefix and a list of encodings.")

(defvar *test-success-counter* 0
  "Counts the number of successful tests.")

(defun create-file-variants (file-name symbol)
  "For a name suffix FILE-NAME and a symbol SYMBOL denoting an
encoding returns a list of pairs where the car is a full file
name and the cdr is the corresponding external format.  This list
contains all possible variants w.r.t. to line-end conversion and
endianness."
  (let ((args (ecase symbol
                (:ascii '(:ascii))
                (:latin1 '(:latin-1))
                (:latin8 '(:hebrew))
                (:cp1252 '(:code-page :id 1252))
                (:koi8r '(:koi8-r))
                (:utf8 '(:utf-8))
                (:ucs2 '(:utf-16))
                (:ucs4 '(:utf-32))))
        (endianp (member symbol '(:ucs2 :ucs4))))
    (loop for little-endian in (if endianp '(t nil) '(t))
          for endian-suffix in (if endianp '("_le" "_be") '(""))
          nconc (loop for eol-style in '(:lf :cr :crlf)
                      collect (cons (format nil "~A_~(~A~)_~(~A~)~A.txt"
                                            file-name symbol eol-style endian-suffix)
                                    (apply #'make-external-format
                                           (append args `(:eol-style ,eol-style
                                                          :little-endian ,little-endian))))))))

(defun create-test-combinations (file-name symbols)
  "For a name suffix FILE-NAME and a list of symbols SYMBOLS
denoting different encodings of the corresponding file returns a
list of lists which can be used as arglists for COMPARE-FILES."
  (let ((file-variants (loop for symbol in symbols
                             nconc (create-file-variants file-name symbol))))
    (loop for (name-in . external-format-in) in file-variants
          nconc (loop for (name-out . external-format-out) in file-variants
                      collect (list name-in external-format-in name-out external-format-out)))))
                      
(defun file-equal (file1 file2)
  "Returns a true value iff FILE1 and FILE2 have the same
contents \(viewed as binary files)."
  (with-open-file (stream1 file1 :element-type 'octet)
    (with-open-file (stream2 file2 :element-type 'octet)
      (and (= (file-length stream1) (file-length stream2))
           (loop for byte1 = (read-byte stream1 nil nil)
                 for byte2 = (read-byte stream2 nil nil)
                 while (and byte1 byte2)
                 always (= byte1 byte2))))))

(defun copy-stream (stream-in external-format-in stream-out external-format-out)
  "Copies the contents of the binary stream STREAM-IN to the
binary stream STREAM-OUT using flexi streams - STREAM-IN is read
with the external format EXTERNAL-FORMAT-IN and STREAM-OUT is
written with EXTERNAL-FORMAT-OUT."
  (let ((in (make-flexi-stream stream-in :external-format external-format-in))
        (out (make-flexi-stream stream-out :external-format external-format-out)))
    (loop for line = (read-line in nil nil)
          while line
          do (write-line line out))))

(defun copy-file (path-in external-format-in path-out external-format-out direction-out direction-in)
  "Copies the contents of the file denoted by the pathname
PATH-IN to the file denoted by the pathname PATH-OUT using flexi
streams - STREAM-IN is read with the external format
EXTERNAL-FORMAT-IN and STREAM-OUT is written with
EXTERNAL-FORMAT-OUT.  The input file is opened with
the :DIRECTION keyword argument DIRECTION-IN, the output file is
opened with the :DIRECTION keyword argument DIRECTION-OUT."
  (with-open-file (in path-in
                      :element-type 'octet
                      :direction direction-in
                      :if-does-not-exist :error
                      :if-exists :overwrite)
    (with-open-file (out path-out
                         :element-type 'octet
                         :direction direction-out
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (copy-stream in external-format-in out external-format-out))))

#+:lispworks
(defun copy-file-lw (path-in external-format-in path-out external-format-out direction-out direction-in)
  "Same as COPY-FILE, but uses character streams instead of
binary streams.  Only used to test LispWorks-specific behaviour."
  (with-open-file (in path-in
                      :external-format '(:latin-1 :eol-style :lf)
                      :direction direction-in
                      :if-does-not-exist :error
                      :if-exists :overwrite)
    (with-open-file (out path-out
                         :external-format '(:latin-1 :eol-style :lf)
                         :direction direction-out
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (copy-stream in external-format-in out external-format-out))))

(defun compare-files (path-in external-format-in path-out external-format-out)
  "Copies the contents of the file (in the `test') denoted by the
relative pathname PATH-IN to the file (in a temporary directory)
denoted by the relative pathname PATH-OUT using flexi streams -
STREAM-IN is read with the external format EXTERNAL-FORMAT-IN and
STREAM-OUT is written with EXTERNAL-FORMAT-OUT.  The resulting
file is compared with an existing file in the `test' directory to
check if the outcome is as expected.  Uses various variants of
the :DIRECTION keyword when opening the files."
  (let ((full-path-in (merge-pathnames path-in *this-file*))
        (full-path-out (ensure-directories-exist
                        (merge-pathnames path-out *tmp-dir*)))
        (full-path-orig (merge-pathnames path-out *this-file*)))
    (dolist (direction-out '(:output :io))
      (dolist (direction-in '(:input :io))
        (format *error-output* "Test ~S ~S [~A]~% --> ~S [~A].~%" path-in
                (flex::normalize-external-format external-format-in) direction-in
                (flex::normalize-external-format external-format-out) direction-out)
        (copy-file full-path-in external-format-in
                   full-path-out external-format-out
                   direction-out direction-in)
        (cond ((file-equal full-path-out full-path-orig)
               (incf *test-success-counter*))
              (t (format *error-output* " Test failed!!!~%")))
        (terpri *error-output*)
        #+:lispworks
        (format *error-output* "LW-Test ~S ~S [~A]~%    --> ~S [~A].~%" path-in
                (flex::normalize-external-format external-format-in) direction-in
                (flex::normalize-external-format external-format-out) direction-out)
        #+:lispworks
        (copy-file full-path-in external-format-in
                   full-path-out external-format-out
                   direction-out direction-in)
        #+:lispworks
        (cond ((file-equal full-path-out full-path-orig)
               (incf *test-success-counter*))
              (t (format *error-output* "    Test failed!!!~%")))
        (terpri *error-output*)))))

(defmacro with-test ((test-description) &body body)
  "Defines a test.  Two utilities are available inside of the body of
the maco: The function FAIL, and the macro CHECK.  FAIL, the lowest
level utility, marks the test defined by WITH-TEST as faided.  CHECK
checks whether its argument is true, otherwise it calls FAIL. If
during evaluation of the specified expression any condition is
signaled, this is also considered a failure.

WITH-TEST prints reports while the tests run.  It also increments
*TEST-SUCCESS-COUNT* if a test completes successfully."
  (flex::with-unique-names (successp)
    `(let ((,successp t))
       (flet ((fail (format-str &rest format-args)
                (setf ,successp nil)
                (apply #'format *error-output* format-str format-args)))
         (macrolet ((check (expression)
                      `(handler-case
                           (unless ,expression
                             (fail "Expression ~S failed.~%" ',expression))
                         (condition (c)
                           (fail "Expression ~S failed signaling condition of type ~A: ~A.~%" 
                                 ',expression (type-of c) c)))))
           (format *error-output* "Test ~S~%" ,test-description)
           ,@body
           (if ,successp
             (incf *test-success-counter*)
             (format *error-output* "    Test failed!!!~%"))
           (terpri *error-output*)
           (terpri *error-output*))
         ,successp))))

(defmacro using-values ((&rest values) &body body)
  "Executes BODY and feeds an element from VALUES to the USE-VALUE
restart each time a FLEXI-STREAM-ENCODING-ERROR is signaled.  Signals
an error when there are more or less FLEXI-STREAM-ENCODING-ERRORs than
there are elements in VALUES."
  (flex::with-unique-names (value-stack condition-counter)
    `(let ((,value-stack ',values)
	   (,condition-counter 0))
       (handler-bind ((flexi-stream-encoding-error
                       #'(lambda (c)
                           (declare (ignore c)) 
                           (unless ,value-stack
                             (error "Too many FLEXI-STREAM-ENCODING-ERRORs signaled, expected only ~A."
                                    ,(length values)))
                           (incf ,condition-counter)
                           (use-value (pop ,value-stack)))))
         (prog1 (progn ,@body)
           (when ,value-stack
             (error "~A FLEXI-STREAM-ENCODING-ERRORs signaled, but ~A were expected."
                    ,condition-counter ,(length values))))))))

(defun read-flexi-line (sequence external-format)
  "Creates and returns a string from the octet sequence SEQUENCE using
the external format EXTERNAL-FORMAT."
  (with-input-from-sequence (in sequence)
    (setq in (make-flexi-stream in :external-format external-format))
    (read-line in)))

(defun encoding-error-handling-test()
  (with-test ("Handling of encoding errors")
    (let ((*substitution-char* #\?))
      ;; :ASCII doesn't have characters with char codes > 127
      (check (string= "a??" (read-flexi-line `(,(char-code #\a) 128 200) :ascii)))
      ;; :WINDOWS-1253 doesn't have a characters with codes 170 and 210
      (check (string= "a??" (read-flexi-line `(,(char-code #\a) 170 210) :windows-1253)))
      ;; not a valid UTF-8 sequence
      (check (string= "??" (read-flexi-line `(#xe4 #xf6 #xfc) :utf8)))
      ;; UTF-8 can't start neither with #b11111110 nor with #b11111111
      (check (string= "??" (read-flexi-line `(#b11111110 #b11111111) :utf8))))
    (let ((*substitution-char* nil))
      ;; :ASCII doesn't have characters with char codes > 127
      (check (string= "abc" (using-values (#\b #\c)
                              (read-flexi-line `(,(char-code #\a) 128 200) :ascii))))
      ;; :WINDOWS-1253 encoding doesn't have a characters with codes 170 and 210
      (check (string= "axy" (using-values (#\x #\y)
                              (read-flexi-line `(,(char-code #\a) 170 210) :windows-1253))))
      ;; not a valid UTF-8 sequence
      (check (string= "QW" (using-values (#\Q #\W) (read-flexi-line `(#xe4 #xf6 #xfc) :utf8))))
      ;; UTF-8 can't start neither with #b11111110 nor with #b11111111
      (check (string= "QW" (using-values (#\Q #\W) (read-flexi-line `(#b11111110 #b11111111) :utf8))))
      ;; only one byte
      (check (string= "E" (using-values (#\E) (read-flexi-line `(#x01) :utf-16le))))
      ;; two bytes, but value of resulting word suggests that another word follows
      (check (string= "R" (using-values (#\R) (read-flexi-line `(#x01 #xd8) :utf-16le))))
      ;; the second word must fit into the [#xdc00; #xdfff] interval, but it is #xdbff
      (check (string= "T" (using-values (#\T) (read-flexi-line `(#x01 #xd8 #xff #xdb) :utf-16le))))
      ;; the same as for little endian above, but using inverse order of bytes in words
      (check (string= "E" (using-values (#\E) (read-flexi-line `(#x01) :utf-16be))))
      (check (string= "R" (using-values (#\R) (read-flexi-line `(#xd8 #x01) :utf-16be))))
      (check (string= "T" (using-values (#\T) (read-flexi-line `(#xd8 #x01 #xdb #xff) :utf-16be))))
      ;; the only case when error is signaled for UTF-32 is at end of file
      ;; in the middle of 4-byte sequence, both for big and little endian
      (check (string= "Y" (using-values (#\Y) (read-flexi-line `(#x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line `(#x01 #x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line `(#x01 #x01 #x01) :utf-32le))))
      (check (string= "aY" (using-values (#\Y)
                             (read-flexi-line `(,(char-code #\a) #x00 #x00 #x00 #x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line `(#x01) :utf-32be))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line `(#x01 #x01) :utf-32be))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line `(#x01 #x01 #x01) :utf-32be))))
      (check (string= "aY" (using-values (#\Y)
                             (read-flexi-line `(#x00 #x00 #x00 ,(char-code #\a) #x01) :utf-32be)))))))

(defun run-tests ()
  "Applies COMPARE-FILES to all test scenarios created with
CREATE-TEST-COMBINATIONS, runs test for handling of encoding errors,
and shows simple statistics at the end."
  (let* ((*test-success-counter* 0)
         (args-list (loop for (file-name symbols) in *test-files*
                          nconc (create-test-combinations file-name symbols)))
         (no-tests (* 4 (length args-list))))
    #+:lispworks
    (setq no-tests (* 2 no-tests))
    (dolist (args args-list)
      (apply #'compare-files args))
    (incf no-tests)
    (encoding-error-handling-test)
    (format *error-output* "~%~%~:[~A of ~A tests failed..~;~*All ~A tests passed~].~%"
            (= no-tests *test-success-counter*) (- no-tests *test-success-counter*) no-tests)))
            
