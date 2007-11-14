;; ledger.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(defpackage :ledger
  (:use :common-lisp :local-time :cambl :cl-ppcre :periods)
  (:export binder
	   binder-commodity-pool
	   binder-root-account
	   binder-journals
	   binder-transactions
	   binder-data

	   journal
	   journal-binder
	   journal-entries
	   journal-source
	   journal-data

	   account
	   account-parent
	   account-children
	   account-name
	   account-fullname
	   account-transactions
	   account-data

	   entry
	   entry-journal
	   entry-actual-date
	   entry-effective-date
	   entry-date
	   entry-status
	   entry-code
	   entry-payee
	   entry-note
	   entry-transactions
	   entry-position
	   entry-data

	   make-transaction
	   transaction
	   xact-entry
	   xact-actual-date
	   xact-effective-date
	   xact-date
	   xact-status
	   xact-account
	   xact-amount
	   xact-note
	   xact-tags
	   xact-virtualp
	   xact-must-balance-p
	   xact-position
	   xact-data
	   xact-cleared-p
	   xact-pending-p
	   xact-uncleared-p

	   make-item-position
	   item-position
	   item-position-begin-char
	   item-position-end-char
	   item-position-begin-line
	   item-position-end-line
	   item-position-source

	   item-status
	   uncleared
	   pending
	   cleared

	   add-transaction
	   add-entry
	   add-journal
	   find-account
	   find-child-account

	   *use-effective-dates*
	   *default-account*
	   *registered-parsers*

	   read-journal-file

	   map-transactions
	   do-transactions

	   read-value-expr

	   destructively-filter
	   normalize-binder
	   abbreviate-string
	   register-report
	   report
	   register
	   calculate-totals
	   read-value-expr
	   compile-value-expr))

(in-package :ledger)

(deftype item-status ()
  '(member uncleared pending cleared))

(defstruct (item-position)
  begin-char
  end-char
  begin-line
  end-line
  source)

(defstruct (transaction
	     (:conc-name xact-)
	     (:print-function print-transaction))
  entry
  (actual-date nil     :type (or datetime null))
  (effective-date nil  :type (or datetime null))
  (status 'uncleared   :type item-status)
  account
  (amount nil	       :type (or amount function null))
  (cost nil	       :type (or amount function null))
  (note nil	       :type (or string null))
  (tags nil)
  (virtual-p nil       :type boolean)
  (must-balance-p t    :type boolean)
  position
  data)

(defun print-transaction (transaction stream depth)
  (declare (ignore depth))
  (print-unreadable-object (transaction stream :type t)
    (format stream "")))

(declaim (inline xact-value))
(defun xact-value (key xact)
  (cdr (assoc key (xact-data xact))))

(defclass entry ()
  ((journal        :accessor entry-journal	   :initarg :journal)
   (actual-date	   :accessor entry-actual-date	   :initarg :actual-date
		   :type datetime)
   (effective-date :accessor entry-effective-date  :initarg :effective-date
		   :initform nil :type (or datetime null))
   (status         :accessor entry-status	   :initarg :status
		   :initform 'uncleared :type item-status)
   (code	   :accessor entry-code		   :initarg :code
		   :initform nil :type (or string null))
   (payee	   :accessor entry-payee	   :initarg :payee
		   :initform nil :type (or string null))
   (note	   :accessor entry-note		   :initarg :note
		   :initform nil :type (or string null))
   (transactions   :accessor entry-transactions	   :initarg :transactions
		   :initform nil)
   position
   data))

(defclass account ()
  ((parent         :accessor account-parent	   :initarg :parent
		   :initform nil)
   (children       :accessor account-children	   :initarg :children
		   :initform nil :type (or hash-table null))
   (name	   :accessor account-name	   :initarg :name
		   :type string)
   (fullname	   :accessor account-fullname	   :initarg :fullname
		   :type string)
   (transactions   :accessor account-transactions  :initarg :transactions
		   :initform nil)
   (last-transaction-cell :accessor account-last-transaction-cell :initform nil)
   data))

(defclass journal ()
  ((binder	   :accessor journal-binder	   :initarg :binder)
   (entries	   :accessor journal-entries	   :initarg :entries
		   :initform nil)
   (last-entry-cell :accessor journal-last-entry-cell :initform nil)
   (source	   :accessor journal-source	   :initarg :source-path
		   :type pathname)
   data))

(defclass binder ()
  ((commodity-pool :accessor binder-commodity-pool :initarg :commodity-pool
		   :type commodity-pool)
   (root-account   :accessor binder-root-account   :initarg :root-account
		   :initform (make-instance 'account :name "") :type account)
   (journals	   :accessor binder-journals	   :initarg :journals
		   :initform nil)
   (transactions   :accessor binder-transactions   :initarg :transactions
		   :initform nil)
   data))

(defgeneric add-transaction (item transaction))
(defgeneric add-entry (journal entry))
(defgeneric add-journal (binder journal))
(defgeneric find-account (item account-path &key create-if-not-exists-p))

;; Textual journal parser

(defvar *use-effective-dates* nil)
(defvar *default-account* nil)
(defvar *registered-parsers* nil)

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

(defun find-child-account (account account-name &key
			   (create-if-not-exists-p nil)
			   (fullname nil))
  (the (or account null)
    (let ((accounts-map (account-children account)))
      (or (and accounts-map
	       (gethash account-name accounts-map))
	  (when create-if-not-exists-p
	    (unless accounts-map
	      (setf (account-children account)
		    (setf accounts-map (make-hash-table :test #'equal))))
	    (setf (gethash account-name accounts-map)
		  (make-instance 'account :parent account
				 :name account-name
				 :fullname fullname)))))))

(defun split-string-at-char (string char)
  "Returns a list of substrings of string
divided by ONE colon each.
Note: Two consecutive colons will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defmethod find-account ((binder binder) (account-path string)
			 &key (create-if-not-exists-p nil))
  (the (or account null)
    (labels ((traverse-accounts (account path-elements fullname)
	       (let ((child-account
		      (find-child-account account (car path-elements)
					  :create-if-not-exists-p
					  create-if-not-exists-p
					  :fullname fullname)))
		 (if child-account
		     (if (cdr path-elements)
			 (traverse-accounts child-account (cdr path-elements)
					    fullname)
			 child-account)))))
      (traverse-accounts (binder-root-account binder)
			 (split-string-at-char account-path #\:)
			 account-path))))

(defmethod find-account ((journal journal) (account-path string)
			 &key (create-if-not-exists-p nil))
  (find-account (journal-binder journal) account-path
		:create-if-not-exists-p create-if-not-exists-p))

(defun read-journal-file (binder path)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (with-open-file (in path :direction :input)
    (let ((start-position (file-position in)))
      (dolist (parser *registered-parsers*)
	(let ((journal (funcall parser in binder)))
	  (if journal
	      (return-from nil journal)
	      (file-position in start-position)))))))

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

(defgeneric map-transactions (callable object))

(defmethod map-transactions (callable (binder binder))
  (let ((xacts (binder-transactions binder)))
    (if xacts
	(mapc callable xacts)
	(dolist (journal (binder-journals binder))
	  (dolist (entry (journal-entries journal))
	    (mapc callable (entry-transactions entry)))))))

(defmethod map-transactions (callable (account account))
  (mapc callable (account-transactions account)))

(defmethod map-transactions (callable (entry entry))
  (mapc callable (entry-transactions entry)))

(defmacro do-transactions ((var object &optional (result nil)) &body body)
  `(block nil
     (map-transactions #'(lambda (,var) ,@body) ,object)
     ,result))

(defun entry-date (entry)
  (declare (type entry entry))
  (if *use-effective-dates*
      (or (entry-effective-date entry)
	  (entry-actual-date entry))
      (entry-actual-date entry)))

(defun xact-date (xact)
  (declare (type transaction xact))
  (if *use-effective-dates*
      (or (xact-effective-date xact)
	  (entry-date (xact-entry xact)))
      (or (xact-actual-date xact)
	  (entry-date (xact-entry xact)))))

(defun xact-cleared-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) 'cleared))

(defun xact-pending-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) 'pending))

(defun xact-uncleared-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) 'uncleared))

;; ledger.lisp ends here
