;; This file contains the parser for textual ledger files

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(defpackage :ledger.textual
  (:use :common-lisp :ledger :cambl :cl-ppcre :periods)
  (:export *directive-handlers*))

(in-package :ledger.textual)

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
			    "(?:~A(?:([^; ~C].*?)(?:(@@?)\\s*(.+?))?))?~A?$")
	   *spacing-regexp* #\Tab *comment-regexp*)))

(defvar *directive-handlers*
  `((#\; . ,#'(lambda (in journal)
		(declare (ignore journal))
		;; comma begins a comment; gobble up the rest of the line
		(read-line in nil)))
    ((#\Newline #\Return) . ,#'(lambda (in journal)
				 (declare (ignore journal))
				 (read-char in nil)))
    (#\A . ,#'(lambda (in journal)
		(read-char in)
		(peek-char t in)
		(setf *default-account*
		      (find-account journal (read-line in)
				    :create-if-not-exists-p t))))
    (#\D . ,#'(lambda (in journal)
		(declare (ignore journal))
		(read-char in)
		(peek-char t in)
		(cambl:read-amount in)))
    (#\N . ,#'(lambda (in journal)
		(declare (ignore journal))
		(read-char in)
		(peek-char t in)
		(let ((commodity
		       (cambl:find-commodity (read-line in)
					     :create-if-not-exists-p t)))
		  (setf (cambl::get-no-market-price-p
			 (cambl::commodity-base commodity)) t))))
    (,#'digit-char-p
     . ,#'(lambda (in journal)
	    (add-entry journal (read-plain-entry in journal))))))

(defun read-transaction (in entry)
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
	    (note (aref groups 7))
	    amount)
	(when (and amount-expr (string/= amount-expr ""))
	  (with-input-from-string (in amount-expr)
	    (setf amount (cambl:read-amount in))
	    (when (peek-char t in nil)
	      (file-position in 0)
	      (setf amount (read-value-expr in)))))
	(let ((virtualp (and open-bracket
			     (string= open-bracket close-bracket))))
	  (make-transaction
	   :entry entry
	   ;;:actual-date
	   ;;:effective-date
	   :status (cond ((string= status "*")
			  'cleared)
			 ((string= status "!")
			  'pending)
			 (t
			  'uncleared))
	   :account (find-account (entry-journal entry) account-name
				  :create-if-not-exists-p t)
	   :amount amount
	   :note note
	   ;;:tags
	   :position (make-item-position :begin-char beg-pos
					 :end-char (file-position in))
	   :virtualp virtualp
	   :must-balance-p (and virtualp
				(string= open-bracket "["))))))))

(defun read-plain-entry (in journal)
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
  (let* ((heading-line (read-line in nil))
	 (groups (and heading-line
		      (nth-value 1 (cl-ppcre:scan-to-strings
				    *entry-heading-scanner* heading-line)))))
    (when groups
      (let ((actual-date (aref groups 0))
	    (effective-date (aref groups 1))
	    (status (aref groups 2))
	    (code (aref groups 3))
	    (payee (aref groups 4))
	    (note (aref groups 5)))
	(let ((entry (make-instance
		      'entry
		      :journal journal
		      :actual-date (cambl:parse-datetime actual-date)
		      :effective-date
		      (and effective-date
			   (cambl:parse-datetime effective-date))
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
	     for transaction = (read-transaction in entry)
	     while transaction do
	     (add-transaction entry transaction)
	     (add-transaction (xact-account transaction) transaction))
	  entry)))))

(defun read-journal (in binder)
  (declare (type stream in))
  (declare (type binder binder))
  (let ((journal (make-instance 'journal :binder binder)))
    (loop
       for c = (peek-char nil in nil)
       while c do
       (let ((handler
	      (cdr
	       (assoc-if
		#'(lambda (key)
		    (cond
		      ((characterp key)
		       (char= c key))
		      ((listp key)
		       (member c key :test #'char=))
		      ((functionp key)
		       (funcall key c))
		      (t
		       (error "Unexpected element in `*directive-handlers*': ~S"
			      key))))
		*directive-handlers*))))
	 (if handler
	     (funcall handler in journal)
	     (progn
	       (format t "Unhandled directive (pos ~D): ~C~%"
		       (file-position in) c)
	       (read-line in nil)))))
    journal))

(pushnew #'read-journal *registered-parsers*)

(provide 'textual)

;; textual.lisp ends here
