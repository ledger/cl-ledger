;; ledger.lisp

(declaim (optimize (safety 3) (debug 3)))

(defpackage :ledger
  (:use :common-lisp :cambl :cl-ppcre)
  (:export read-journal-file))

(in-package :ledger)

(defclass transaction ()
  ((parent-entry   :accessor parent-entry   :initarg :parent-entry)
   (actual-date	   :accessor actual-date    :initarg :actual-date)
   (effective-date :accessor effective-date :initarg :effective-date)
   (status	   :accessor status	    :initarg :status)
   (account	   :accessor account	    :initarg :account)
   (amount	   :accessor get-amount	    :initarg :amount)
   (comment	   :accessor comment	    :initarg :comment)
   (tags	   :accessor tags	    :initarg :tags)
   (virtual-p	   :accessor virtual-p	    :initarg :virtual-p)
   (must-balance-p :accessor must-balance-p :initarg :must-balance-p)))

(defclass entry ()
  ((parent-journal :accessor parent-journal :initarg :parent-journal)
   (actual-date	   :accessor actual-date    :initarg :actual-date)
   (effective-date :accessor effective-date :initarg :effective-date)
   (entry-status   :accessor entry-status   :initarg :entry-status)
   (entry-code	   :accessor entry-code	    :initarg :entry-code)
   (payee	   :accessor payee	    :initarg :payee)
   (comment	   :accessor comment	    :initarg :comment)
   (transactions   :accessor transactions   :initarg :transactions)))

(defclass account ()
  ((parent         :accessor parent-account :initarg :parent)
   (name	   :accessor account-name   :initarg :name)
   (transactions   :accessor transactions   :initarg :transactions)))

(defclass journal ()
  ((entries	   :accessor entries	    :initarg :entries)
   (accounts-map   :accessor accounts-map   :initarg :accounts-map)))

;; Textual journal parser

(defvar *date-regexp* "[0-9]{4}[-./][0-9]{2}[-./][0-9]{2}")

(defvar *spacing-regexp* "(?:  |\#Tab| \#Tab)\\s*")

(defvar *comment-regexp* "(?:~A;(.+))?")

(defvar *entry-heading-scanner*
  (cl-ppcre:create-scanner
   (format nil "^(?:(~A)(?:=(~A))?)\\s+(?:(\\*|!)\\s*)?(?:\\((.+?)\\)\\s*)?(.+?)~A$"
	   *date-regexp* *date-regexp* *spacing-regexp* *comment-regexp*)))

(defvar *transaction-scanner*
  (cl-ppcre:create-scanner
   (format nil "^\\s+(?:(\\*|!)\\s*)?([\\[(])?(.+?)([\\])])?~A(?:(.+?)(?:(@@?)\\s*(.+?))?)~A$"
	   *spacing-regexp* *spacing-regexp* *comment-regexp*)))

(defun read-transaction (entry in)
  (declare (type stream in))
  (format t "read-transaction~%")
  (let* ((xact-line (read-line in))
	 (groups (nth-value 1 (cl-ppcre:scan-to-strings
			       *transaction-scanner* xact-line))))
    (format t "xact-line: '~A'~%groups: ~S~%" xact-line groups)
    (when groups
      (let ((xact (make-instance 'transaction :entry entry))
	    (status (aref groups 0))
	    (open-bracket (aref groups 1))
	    (account-name (aref groups 2))
	    (close-bracket (aref groups 3))
	    (amount-expr (aref groups 4))
	    (cost-specifier (aref groups 5))
	    (cost-expr (aref groups 6))
	    (comment (aref groups 7)))
	xact))))

(defmethod add-transaction ((entry entry) (transaction transaction))
  )

(defun read-plain-entry (in)
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
  (format t "read-plain-entry~%")
  (let* ((heading-line (read-line in))
	 (groups (nth-value 1 (cl-ppcre:scan-to-strings
			       *entry-heading-scanner* heading-line))))
    (format t "heading-line: '~A'~%groups: ~S~%" heading-line groups)
    (when groups
      (let ((entry (make-instance 'entry))
	    (actual-date (aref groups 0))
	    (effective-date (aref groups 1))
	    (entry-status (aref groups 2))
	    (entry-code (aref groups 3))
	    (payee (aref groups 4))
	    (comment (aref groups 5)))
	(loop
	   (let ((transaction (read-transaction entry in)))
	     (if transaction
		 entry		       ;(append-transaction entry transaction)
		 (return))))))))

(defun read-textual-journal (in)
  (declare (type stream in))
  (let ((bolp t))
   (loop
      for c = (read-char in nil)
      while c do
      (cond ((char-equal c #\;)
	     ;; comma begins a comment; gobble up the rest of the line
	     (read-line in)
	     (setq bolp t))
	    ((or (char-equal c #\Newline)
		 (char-equal c #\Return))
	     (setq bolp t))
	    ((and bolp (digit-char-p c))
	     (unread-char c in)
	     (read-plain-entry in)
	     (setq bolp t))
	    (t
	     (setq bolp nil))))))

(defun read-journal-file (path)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (with-open-file (in path :direction :input)
    (read-textual-journal in)))

;; ledger.lisp ends here
