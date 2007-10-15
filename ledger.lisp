;; ledger.lisp

(declaim (optimize debug))

(defpackage :LEDGER
  (:use
   :COMMON-LISP
   ;;:CAMBL
   :CL-PPCRE))

(defclass journal ()
  (entries-list
   accounts-mapping))

(defclass entry ()
  (parent-journal
   actual-date
   effective-date
   entry-status
   entry-code
   payee
   comment
   transactions-list))

(defclass account ()
  (name
   transactions-list))

(defclass transaction ()
  (parent-entry
   actual-date
   effective-date
   status
   account
   amount
   comment
   tags
   virtual-p
   must-balance-p))

(defmethod append-transaction ((entry entry) (transaction transaction)))

;; Textual journal parser

(defvar *date-regexp* "[0-9]{4}[-./][0-9]{2}[-./][0-9]{2}")

(defvar *spacing-regexp* "(?:  |\t| \t)\\s+")

(defvar *entry-heading-scanner*
  (cl-ppcre:create-scanner
   (format nil "(?:(~a)(?:=(~a))?)(?:\\s+(\\*|!))?(?:\\s+\\((.+?)\\))?\\s+(.+)(?:~a;(.+))?"
	   *date-regexp* *date-regexp* *spacing-regexp*)))

(defun read-transaction-from-stream (in)
  (declare (type stream in))
  ;; jww (2007-10-14): TODO
  (input-stream-p in)
  (values))

(defun read-entry-from-stream (in)
  "Read in the header line for the entry, which has the syntax:
  
    (DATE(=DATE)?)( (*|!))?( (\((.+?)\)))? (.+)(:spacer:;(.+))?
  
  :spacer: means: two spaces, a tab, or a space and a tab, followed by any
  amount of whitespace.
  
  The groups identified in this regular expression (found in the scanner
  *entry-heading-scanner*) have these meanings:
  
  1 - The actual date of the entry.
  2 - The (optional) effective date of the entry.
  4 - The (optional) status of the entry: *=cleared, !=pending.
  6 - The (optional) \"code\" for the entry; has no meaning to Ledger.
  7 - The payee or description of the entry.
  9 - A comment giving further details about the entry."
  (declare (type stream in))
  (let ((entry (make-instance 'entry)))
    (let* ((heading-line (read-line in))
	   (groups (nth-value 1 (cl-ppcre:scan-to-strings
				 *entry-heading-scanner* heading-line)))
	   (actual-date (aref groups 0))
	   (effective-date (aref groups 1))
	   (entry-status (aref groups 2))
	   (entry-code (aref groups 3))
	   (payee (aref groups 4)))
      (if actual-date
	  (format t "actual-date (~a) " actual-date))
      (if effective-date
	  (format t "effective-date (~a) " effective-date))
      (if entry-status
	  (format t "entry-status (~a) " entry-status))
      (if entry-code
	  (format t "entry-code (~a) " entry-code))
      (if payee
	  (format t "payee (~a) " payee)))
    (loop
       (let ((transaction (read-transaction-from-stream in)))
	 (if transaction
	     entry;(append-transaction entry transaction)
	     (return))))))

(defun read-journal-from-stream (in)
  (declare (type stream in))
  (loop
     (let ((c (read-char in nil)))
       (cond ((char-equal c #\;)
	      ;; comma begins a comment; gobble up the rest of the line
	      (read-line in))
	     ((or (char-equal c #\Newline)
		  (char-equal c #\Return)))
	     ((digit-char-p c)
	      (read-entry-from-stream in))
	     (t (return))))))

(defun read-journal-text (path)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (with-open-file (in path :direction :input)
    (read-journal-from-stream in)))

;; ledger.lisp ends here
