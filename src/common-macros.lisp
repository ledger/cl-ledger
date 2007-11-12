;; -*- mode: lisp -*-

;; common-macros.lisp - Macros for Common Lisp reminiscent of Python or Ruby
;;
;; This is version: 0.1 (2007/10/26)

;;;_* Copyright (c) 2007, John Wiegley.  All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;;
;; - Neither the name of New Artisans LLC nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;_* TODO

;; For each idiom that is translated, include the code from Python/Ruby/Groovy
;; wherever upon which it is based.
;;
;; Idioms to cover (and all must be entirely cross platform):
;;
;; - Looping through lines in a file, stream or string
;; - Reading and walking directories
;; - Platform-independent timezones
;; - Local time <-> universal time conversion (see Naggum's local-time code)
;; - Parsing of dates and times
;; - An independent "date" type
;; - Working with the Gregorian calendar
;; - A type to deal with time intervals
;; - stat-like access to file details (abstract sb-posix)
;; - Streaming of plain CLOS objects
;; - Persisting of plain CLOS objects
;;   This doesn't need to be a database per se (though it could be), it just
;;   need be an incredibly fast and easy way to put an object away and get
;;   back to it later.
;;
;; Each of these should sport: a macro (like with- or do-), a high-level
;; function (like map), and an iterator.
;;
;; A bit beyond the scope of this tiny package, but also needed:
;;
;; - A hierarchy of collection classes (see CL-Container)
;; - Algorithms to work on the same (ala the STL or similar)
;; - Iterators
;; - Generators
;; - Builders (ala Groovy)
;;
;; * Look at http://weitz.de/cl-fad/ for cross-platform pathnames.

;;;_* Commentary

;; This package provides some simple functions and macros that offer
;; facilities to Python and Ruby programmers closer to what they're familiar
;; with.  Here are some quick examples you can try right away:
;;
;; (list-directory "/tmp")
;;
;;   Returns a list of every item in the given directory.  Everything is
;;   returned except for the . and .. entries.  This function always returns
;;   full pathnames.  Thanks to Peter Seibel for writing this (in his book,
;;   "Practical Common Lisp").
;; 
;; (with-each-file-in-directory (file "/tmp")
;;   (princ file) (terpri))
;;
;;   Execute the given body of code for every file in the stated directory.
;;   The directory name does not have to end with a /, as it usually would in
;;   CL programs.  Note the basic format of this macro:
;; 
;;     (with-each-file-in-directory (VARNAME DIRECTORY)
;;       BODY-FORMS...)
;;
;; (with-each-basename-in-directory (file "/tmp")
;;   (princ file) (terpri))
;;
;;   This is like `with-each-file-in-directory', except that the variable is
;;   bound with only the basename for each file, not its full path.
;; 
;; (walk-directory "/tmp"
;; 		#'(lambda (file)
;; 		    (princ file) (terpri)))
;;
;;   Recursive walk the given directory, calling the specified function (or
;;   closure) for each file found.  If another closure is given, it will be
;;   called for every directory.  If the keyword argument :DIRECTORIES is
;;   present, then the first closure is called for every file *and* every
;;   directory (in case you just want One Closure To Rule Them All).  The full
;;   format of this function is:
;;
;;     (walk-directory PATHNAME FILE-CLOSURE [DIRECTORY-CLOSURE]
;;                     [:DIRECTORIES] [:TEST TEST-CLOSURE])
;;
;;   If :TEST is specified, it should be a closure that takes a full pathname
;;   (either file or directory), and returns a generalized boolean to decide
;;   whether or not the other closures should be called.
;; 
;; (with-each-line-in-file (line "/etc/passwd")
;;   (princ line) (terpri))
;;
;;   This is analogous to "for line in open('/etc/passwd'): print line" in
;;   Python.
;; 
;;     (with-open-file (input "/etc/passwd")
;;       (with-each-line-in-stream (line input)
;;         (princ line) (terpri)))
;;
;;   `with-each-line-in-stream' provides a stream based variant of the same
;;   functionality.  This makes it easy to walk lines inside a string:
;;
;;     (with-input-from-string (stream "Some big, multi-line string")
;;       (with-each-line-in-stream (line stream)
;;         (princ line) (terpri)))

(defpackage :common-macros
  (:use :cl)
  (:export file-exists-p))

(in-package :common-macros)

(defun file-exists-p (pathname)
  (probe-file pathname))

(defun file-truename (pathname)
  (probe-file pathname))

(defmacro with-each-line-in-file (var-and-file &rest body-forms)
  `(with-open-file (file-stream ,(nth 1 var-and-file))
     (loop for ,(nth 0 var-and-file) = (read-line file-stream nil)
	while ,(nth 0 var-and-file)
	do ,@body-forms)))

(defmacro with-each-line-in-stream (var-and-stream &rest body-forms)
  `(loop for ,(nth 0 var-and-stream) =
	(read-line ,(nth 1 var-and-stream) nil)
      while ,(nth 0 var-and-stream)
      do ,@body-forms))

;; These functions are by Peter Seibel, from his book "Practical Common Lisp".
;; I've made some small changes to `walk-directory'.

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name      nil
	 :type      nil
	 :defaults pathname)
	pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defmacro with-each-file-in-directory (var-and-dir &rest body-forms)
  `(loop for ,(nth 0 var-and-dir)
      in (list-directory ,(nth 1 var-and-dir))
      when (not (directory-pathname-p ,(nth 0 var-and-dir)))
      do ,@body-forms))

(defmacro with-each-item-in-directory (var-and-dir &rest body-forms)
  `(loop for ,(nth 0 var-and-dir)
      in (list-directory ,(nth 1 var-and-dir))
      do ,@body-forms))

(defmacro with-each-basename-in-directory (var-and-dir &rest body-forms)
  `(loop for ,(nth 0 var-and-dir)
      in (list-directory ,(nth 1 var-and-dir))
      when (not (directory-pathname-p ,(nth 0 var-and-dir)))
      do (progn
	   (setf ,(nth 0 var-and-dir)
		 (file-namestring ,(nth 0 var-and-dir)))
	   ,@body-forms)))

(defun walk-directory (dirname file-fn &optional dir-fn
		       &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
	    (when (funcall test name)
	      (if directories
		  (funcall file-fn name)
		  (if dir-fn
		      (funcall dir-fn name))))
            (dolist (x (list-directory name))
	      (walk x)))
           ((funcall test name)
	    (funcall file-fn name)))))
    (walk (pathname-as-directory dirname))))

;; common-macros.lisp ends here
