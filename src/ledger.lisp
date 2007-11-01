;; ledger.lisp

(declaim (optimize (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cambl)
  (require :cl-ppcre))

(defpackage :ledger
  (:use :common-lisp :cambl :cl-ppcre)
  (:export binder
	   binder-journals
	   journal
	   journal-entries
	   entry
	   entry-payee
	   entry-transactions
	   transaction
	   xact-amount))

(in-package :ledger)

(deftype item-status ()
  '(member uncleared pending cleared))

(defstruct (transaction
	     (:conc-name xact-)
	     (:print-function print-transaction))
  entry
  (date nil	       :type (or datetime null))
  (effective-date nil  :type (or datetime null))
  (status 'uncleared   :type item-status)
  account
  (amount nil	       :type amount)
  (note nil	       :type (or string null))
  (tags nil)
  (stream-position nil :type (or integer null))
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
   (transactions   :accessor entry-transactions	   :initarg :transactions
		   :initform nil)))

(defclass account ()
  ((parent         :accessor account-parent	   :initarg :parent
		   :initform nil)
   (children       :accessor account-children	   :initarg :children
		   :initform nil :type (or hash-table null))
   (name	   :accessor account-name	   :initarg :name
		   :type string)
   (transactions   :accessor account-transactions  :initarg :transactions
		   :initform nil)
   (last-transaction-cell :accessor account-last-transaction-cell :initform nil)))

(defclass journal ()
  ((binder	   :accessor journal-binder	   :initarg :binder)
   (entries	   :accessor journal-entries	   :initarg :entries
		   :initform nil)
   (last-entry-cell :accessor journal-last-entry-cell :initform nil)
   (source-path	   :accessor journal-source-path   :initarg :source-path
		   :type pathname)))

(defclass binder ()
  ((commodity-pool :accessor binder-commodity-pool :initarg :commodity-pool
		   :type commodity-pool)
   (root-account   :accessor binder-root-account   :initarg :root-account
		   :initform (make-instance 'account :name "") :type account)
   (journals	   :accessor binder-journals	   :initarg :journals
		   :initform nil)))

(defgeneric add-transaction (item transaction))
(defgeneric add-entry (journal entry))
(defgeneric add-journal (binder journal))
(defgeneric find-child-account (account account-name &key create-if-not-exists-p))
(defgeneric find-account (item account-path &key create-if-not-exists-p))

;; Textual journal parser

(defvar *date-regexp* "[0-9]{4}[-./][0-9]{2}[-./][0-9]{2}")

(defvar *spacing-regexp* (format nil "(?:  |~C| ~C)\\s*" #\Tab #\Tab))

(defvar *comment-regexp* (format nil "(?:~A;(.+))?" *spacing-regexp*))

(defvar *entry-heading-scanner*
  (cl-ppcre:create-scanner
   (format nil (concatenate 'string
			    "^(?:(~A)(?:=(~A))?)\\s+(?:(\\*|!)\\s*)?"
			    "(?:\\((.+?)\\)\\s+)?(.+?)~A$")
	   *date-regexp* *date-regexp* *comment-regexp*)))

(defvar *transaction-scanner*
  (cl-ppcre:create-scanner
   (format nil (concatenate 'string
			    "^\\s+(?:(\\*|!)\\s*)?([\\[(])?(.+?)([\\])])?"
			    "~A(?:(.+?)(?:(@@?)\\s*(.+?))?)~A$")
	   *spacing-regexp* *comment-regexp*)))

(defun read-transaction (entry in)
  (declare (type stream in))
  ;;(format t "read-transaction~%")
  (let* ((beg-pos (file-position in))
	 (xact-line (read-line in nil))
	 (groups (and xact-line
		      (nth-value 1 (cl-ppcre:scan-to-strings
				    *transaction-scanner* xact-line)))))
    ;;(format t "xact-line: '~A'~%groups: ~S~%" xact-line groups)
    (when groups
      (let ((status (aref groups 0))
	    (open-bracket (aref groups 1))
	    (account-name (aref groups 2))
	    (close-bracket (aref groups 3))
	    (amount-expr (aref groups 4))
	    ;;(cost-specifier (aref groups 5))
	    ;;(cost-expr (aref groups 6))
	    (note (aref groups 7)))
	(let ((virtual-p (and (string/= open-bracket "")
			      (string= open-bracket close-bracket))))
	  (make-transaction
	   :entry entry
	   ;;:date
	   ;;:effective-date
	   :status (cond ((string= status "*")
			  'cleared)
			 ((string= status "!")
			  'pending)
			 (t
			  'uncleared))
	   :account (find-account (entry-journal entry) account-name
				  :create-if-not-exists-p t)
	   :amount (and (string/= amount-expr "")
			(cambl:amount amount-expr))
	   :note note
	   ;;:tags
	   :stream-position beg-pos
	   :virtual-p virtual-p
	   :must-balance-p (and virtual-p
				(zerop (string/= open-bracket "[")))))))))

(defmethod add-transaction ((entry entry) (transaction transaction))
  (pushend transaction (entry-transactions entry)))

(defmethod add-transaction ((account account) (transaction transaction))
  (pushend transaction (account-transactions account)
	   (account-last-transaction-cell account)))

(defmethod add-entry ((journal journal) (entry entry))
  (pushend entry (journal-entries journal)
	   (journal-last-entry-cell journal)))

(defmethod add-journal ((binder binder) (journal journal))
  (pushend journal (binder-journals binder)))

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
  (the (or account null)
    (let ((accounts-map (account-children account)))
      (or (and accounts-map
	       (gethash account-name accounts-map))
	  (when create-if-not-exists-p
	    (unless accounts-map
	      (setf (account-children account)
		    (setq accounts-map (make-hash-table :test #'equal))))
	    (setf (gethash account-name accounts-map)
		  (make-instance 'account :parent account
				 :name account-name)))))))

(defmethod find-account ((binder binder) (account-path string)
			 &key (create-if-not-exists-p nil))
  (the (or account null)
    (labels ((traverse-accounts (account path-elements)
	       (let ((child-account
		      (find-child-account account (car path-elements)
					  :create-if-not-exists-p
					  create-if-not-exists-p)))
		 (if child-account
		     (if (cdr path-elements)
			 (traverse-accounts child-account (cdr path-elements))
			 child-account)))))
      (traverse-accounts (binder-root-account binder)
			 (split-by-colon account-path)))))

(defmethod find-account ((journal journal) (account-path string)
			 &key (create-if-not-exists-p nil))
  (find-account (journal-binder journal) account-path
		:create-if-not-exists-p create-if-not-exists-p))

(defun read-plain-entry (journal in)
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
  (declare (type journal journal))
  (declare (type stream in))
  ;;(format t "read-plain-entry~%")
  (let* ((heading-line (read-line in nil))
	 (groups (and heading-line
		      (nth-value 1 (cl-ppcre:scan-to-strings
				    *entry-heading-scanner* heading-line)))))
    ;;(format t "heading-line: '~A'~%groups: ~S~%" heading-line groups)
    (when groups
      (let (;;(actual-date (aref groups 0))
	    ;;(effective-date (aref groups 1))
	    (status (aref groups 2))
	    (code (aref groups 3))
	    (payee (aref groups 4))
	    (note (aref groups 5)))
	(let ((entry (make-instance
		      'entry
		      :journal journal
		      ;;:date actual-date   ; jww (2007-10-31): need parse-time
		      ;;:effective-date
		      :status (cond ((string= status "*")
				     'cleared)
				    ((string= status "!")
				     'pending)
				    (t
				     'uncleared))
		      :code code
		      :payee payee
		      :note note)))
	  (loop
	     for transaction = (read-transaction entry in)
	     while transaction do
	     (add-transaction entry transaction)
	     (add-transaction (xact-account transaction) transaction))
	  entry)))))

(defun read-textual-journal (binder in)
  (declare (type stream in))
  (let ((bolp t)
	(journal (make-instance 'journal :binder binder)))
    (loop
       for c = (read-char in nil)
       while c do
       (cond ((char-equal c #\;)
	      ;; comma begins a comment; gobble up the rest of the line
	      (read-line in nil)
	      (setq bolp t))
	     ((or (char-equal c #\Newline)
		  (char-equal c #\Return))
	      (setq bolp t))
	     ((and bolp (digit-char-p c))
	      (unread-char c in)
	      (loop
		 for entry = (read-plain-entry journal in)
		 while entry do
		 (add-entry journal entry))
	      (setq bolp t))
	     (t
	      (setq bolp nil))))
    journal))

(defun read-journal-file (binder path)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (with-open-file (in path :direction :input)
    (read-textual-journal binder in)))

(defun binder (&rest journals-or-paths-or-strings)
  (let ((binder (make-instance 'binder
			       :commodity-pool *default-commodity-pool*)))
    (dolist (item journals-or-paths-or-strings)
      (cond ((typep item 'journal)
	     (add-journal binder item))
	    ((typep item '(or pathname string))
	     (let ((journal (read-journal-file binder item)))
	       (if journal
		   (add-journal binder journal))))
	    (t
	     (error "unknown"))))
    binder))

;; ledger.lisp ends here
