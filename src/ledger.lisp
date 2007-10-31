;; ledger.lisp

(declaim (optimize (safety 3) (debug 3)))

(defpackage :ledger
  (:use :common-lisp :cambl :cl-ppcre)
  (:export read-journal-file))

(in-package :ledger)

(deftype item-status ()
  '(member uncleared pending cleared))

(defstruct (transaction (:print-function print-transaction))
  entry
  (date nil	       :type datetime)
  (effective-date nil  :type datetime)
  (status 'uncleared   :type item-status)
  account
  (amount nil	       :type amount)
  (note nil	       :type string)
  (tags nil)
  (stream-position nil :type integer)
  (virtual-p nil       :type boolean)
  (must-balance-p t    :type boolean))

(defun print-transaction (transaction stream depth)
  (declare (ignore depth))
  (print-unreadable-object (transaction stream :type t)
    (format stream "")))

(defclass entry ()
  ((journal        :accessor entry-journal	   :initarg :journal)
   (date	   :accessor entry-date		   :initarg :date
		   :type datetime)
   (effective-date :accessor entry-effective-date  :initarg :effective-date
		   :initform nil :type (or datetime null))
   (entry-status   :accessor entry-status	   :initarg :status
		   :initform 'uncleared :type item-status)
   (entry-code	   :accessor entry-code		   :initarg :code
		   :initform nil :type (or string null))
   (payee	   :accessor entry-payee	   :initarg :payee
		   :type string)
   (comment	   :accessor entry-note		   :initarg :note
		   :initform nil :type (or string null))
   (transactions   :accessor entry-transactions	   :initarg :transactions)))

(defclass account ()
  ((parent         :accessor account-parent	   :initarg :parent
		   :initform nil :type (or account null))
   (children       :accessor account-children	   :initarg :children
		   :initform nil :type (or hash-table null))
   (name	   :accessor account-name	   :initarg :name
		   :type string)
   (transactions   :accessor account-transactions  :initarg :transactions
		   :initform nil)))

(defclass journal ()
  ((binder	   :accessor journal-binder	   :initarg :binder)
   (entries	   :accessor journal-entries	   :initarg :entries)
   (source-path	   :accessor journal-source-path   :initarg :source-path
		   :type pathname)))

(defclass binder ()
  ((commodity-pool :accessor binder-commodity-pool :initarg :commodity-pool
		   :type commodity-pool)
   (root-account   :accessor binder-root-account   :initarg :root-account
		   :type account)
   (journals	   :accessor binder-journals	   :initarg :journals)))

(defgeneric add-transaction (item transaction))
(defgeneric add-entry (journal entry))
(defgeneric add-journal (binder journal))
(defgeneric find-child-account (account account-name &key create-if-not-exists-p))
(defgeneric find-account (binder account-path &key create-if-not-exists-p))
(defgeneric find-account (journal account-path &key create-if-not-exists-p))

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
	(make-transaction :entry entry
			  ;;:date
			  ;;:effective-date
			  :status
			  :account
			  :amount
			  :note
			  :tags
			  :stream-position
			  :virtual-p
			  :must-balance-p)))))

(defmethod add-transaction ((entry entry) (transaction transaction))
  )

(defmethod add-transaction ((account account) (transaction transaction))
  )

(defmethod add-entry ((journal journal) (entry entry))
  )

(defmethod add-journal ((binder binder) (journal journal))
  )

(defun split-by-colon (string)
  "Returns a list of substrings of string
divided by ONE colon each.
Note: Two consecutive colons will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position #\: string :start i)
     collect (subseq string i j)
     while j))

(defmethod find-child-account ((account account) (account-name string)
			       &key (create-if-not-exists-p nil))
  (let ((accounts-map (account-children account)))
    (or (and accounts-map
	     (gethash account-name accounts-map))
	(when create-if-not-exists-p
	  (unless accounts-map
	    (setf (account-children account)
		  (setq accounts-map (make-hash-table :test #'string=))))
	  (setf (gethash account-name accounts-map)
		(make-instance 'account :parent account
			       :name account-name))))))

(defmethod find-account ((binder binder) (account-path string)
			 &key (create-if-not-exists-p nil))
  (labels ((traverse-accounts (account path-elements)
	     (let ((child-account
		    (find-child-account account (car path-elements)
					:create-if-not-exists-p
					create-if-not-exists-p)))
	       (if child-account
		   (if path-elements
		       (traverse-accounts child-account (cdr path-elements))
		       child-account)))))
    (traverse-accounts (binder-root-account binder)
		       (split-by-colon account-path))))

(defmethod find-account ((journal journal) (account-path string)
			 &key (create-if-not-exists-p nil))
  (find-account (journal-binder journal) account-path
		:create-if-not-exists-p create-if-not-exists-p))

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
