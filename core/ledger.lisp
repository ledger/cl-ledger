;; ledger.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(require 'types)

;;;_ * Global variables

(defvar *use-effective-dates* nil)
(defvar *registered-parsers* nil)
(defvar *allow-embedded-lisp* nil)

;;;_ * Journals

(defun read-journal (binder path)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (with-open-file (in path :direction :input)
    (let ((start-position (file-position in)))
      (dolist (parser *registered-parsers*)
	(let ((journal (funcall parser in binder)))
	  (when journal
	    (setf (journal-read-date journal)
		  (file-write-date path))
	    (return-from read-journal journal))))
      (file-position in start-position)
      nil)))

(defun add-journal-from-path (binder path)
  (let ((journal (read-journal binder path)))
    (when journal
      (setf (journal-source journal)
	    (if (stringp path)
		(pathname path)
		path))
      (add-journal binder journal))))

(defmethod add-journal ((binder binder) (journal journal))
  (dolist (j (binder-journals binder))
    (if (eq j journal)
	(return-from add-journal)))
  (pushend journal (binder-journals binder))
  (setf (journal-binder journal) binder))

(defmethod add-journal ((binder binder) (path-string string))
  (let ((path (pathname path-string)))
    (dolist (j (binder-journals binder))
      (if (equal path (journal-source j))
	  (return-from add-journal)))
    (add-journal-from-path binder path)))

(defmethod add-journal ((binder binder) (path pathname))
  (dolist (j (binder-journals binder))
    (if (equal path (journal-source j))
	(return-from add-journal)))
  (add-journal-from-path binder path))

(defmethod add-journal ((journal journal) (child journal))
  (pushend child (journal-contents journal)
	   (journal-last-content-cell journal)))

(declaim (inline add-to-contents))
(defun add-to-contents (journal item)
  (declare (type journal journal))
  (pushend item (journal-contents journal)
	   (journal-last-content-cell journal)))

(declaim (inline parse-journal-date))
(defun parse-journal-date (journal string)
  (strptime string :format (or (journal-date-format journal)
			       *input-time-format*)
	    :default-year (journal-default-year journal)))

;;;_ * Accounts

(declaim (inline account-value))
(defun account-value (account key)
  (let ((value-cell (assoc key (account-data account))))
    (values (cdr value-cell) value-cell)))

(declaim (inline account-value))
(defun account-set-value (account key value)
  (let ((value-cell (assoc key (account-data account))))
    (if value-cell
	(rplacd value-cell value)
	(push (cons key value) (account-data account)))))

(defun reset-accounts (binder)
  (labels ((undo-filter-in-account (name account)
	     (declare (ignore name))
	     (setf (account-data account) nil)
	     (let ((children (account-children account)))
	       (if children
		   (maphash #'undo-filter-in-account children)))))
    (undo-filter-in-account "" (binder-root-account binder)))
  binder)

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

;;;_ * Entries

(defun entry-date (entry)
  (declare (type entry entry))
  (if *use-effective-dates*
      (or (entry-effective-date entry)
	  (entry-actual-date entry))
      (entry-actual-date entry)))

;;;_ * Code to walk the LEDGER object tree

(defmethod entries-iterator ((binder binder))
  (let* ((journals-iterator (list-iterator (binder-journals binder)))
	 (journal (funcall journals-iterator))
	 (entries-iterator (entries-iterator journal)))
    (lambda ()
      (when journal
	(labels
	    ((next-entry ()
	       (let ((item (funcall entries-iterator)))
		 (or item
		     (progn
		       ;; It would be highly unusual to have several (indeed
		       ;; any) journals without entries, so I'm not afraid of
		       ;; much recursion happening here.
		       (setf journal (funcall journals-iterator))
		       (when journal
			 (setf entries-iterator
			       (entries-iterator journal))
			 (next-entry)))))))
	  (next-entry))))))

(defmethod entries-iterator ((journal journal))
  (declare (type journal journal))
  (let ((contents-iterators
	 (list (list-iterator (journal-contents journal))))
	(entry-class (find-class 'entry)))
    (lambda ()
      (loop
	 while contents-iterators
	 for item = (funcall (first contents-iterators))
	 if (null item)
	 do (setf contents-iterators
		  (cdr contents-iterators))
	 else if (eq (class-of item) entry-class)
	 return item
	 if (typep item 'journal)
	 do (push (list-iterator (journal-contents item))
		  contents-iterators)))))

(defmethod entries-iterator ((entry entry))
  (list-iterator (list entry)))

(defun entries-list (object)
  (loop
     with iterator = (entries-iterator object)
     for entry = (funcall iterator)
     while entry collect entry))

(defmacro map-entries (callable object)
  `(map-iterator ,callable (entries-iterator ,object)))

(defmacro do-entries ((var object &optional (result nil)) &body body)
  `(block nil
     (map-entries #'(lambda (,var) ,@body) ,object)
     ,result))

(declaim (inline scan-entries))
(defun scan-entries (object)
  (declare (optimizable-series-function))
  (multiple-value-bind (entries)
      (map-fn '(or entry null) (entries-iterator object))
    (until-if #'null entries)))

;;;_ * Transactions

(defmethod add-transaction ((entry entry) (transaction transaction))
  ;; jww (2007-11-28): This might be a bottleneck
  (pushend transaction (entry-transactions entry)))

(defmethod transactions-iterator ((binder binder) &optional entry-transform)
  (let ((journals-iterator (list-iterator (binder-journals binder)))
	(xacts-iterator (constantly nil)))
    (lambda ()
      (labels
	  ((next-xact ()
	     (or (funcall xacts-iterator)
		 (let ((next-journal (funcall journals-iterator)))
		   (when next-journal
		     (setf xacts-iterator
			   (transactions-iterator next-journal
						  entry-transform))
		     (next-xact))))))
	(next-xact)))))

(defmethod transactions-iterator ((journal journal) &optional entry-transform)
  (let ((entries-iterator (entries-iterator journal))
	(xacts-iterator (constantly nil)))
    (lambda ()
      (labels
	  ((next-xact ()
	     (or (funcall xacts-iterator)
		 (let ((next-entry (funcall entries-iterator)))
		   (when next-entry
		     (setf xacts-iterator
			   (transactions-iterator next-entry
						  entry-transform))
		     (next-xact))))))
	(next-xact)))))

(defmethod transactions-iterator ((entry entry) &optional entry-transform)
  (declare (type (or function null) entry-transform))
  (list-iterator (entry-transactions (if entry-transform
					 (funcall entry-transform entry)
					 entry))))

(defmethod transactions-iterator ((transaction transaction)
				  &optional entry-transform)
  (declare (ignore entry-transform))
  (list-iterator (list transaction)))

(defun transactions-list (object &key (entry-transform nil))
  (loop
     with iterator = (transactions-iterator object entry-transform)
     for xact = (funcall iterator)
     while xact collect xact))

(defmacro map-transactions (callable object &key (entry-transform nil))
  `(map-iterator ,callable (transactions-iterator ,object ,entry-transform)))

;; jww (2007-11-19): deprecated?
(defmacro do-transactions ((var object &optional (result nil)) &body body)
  (let ((iterator (gensym)))
    `(loop
	with ,iterator = (transactions-iterator ,object)
	for ,var = (funcall ,iterator)
	while ,var do (progn ,@body ,result))))

(declaim (inline scan-transactions))
(defun scan-transactions (object &optional entry-transform)
  (declare (optimizable-series-function))
  (multiple-value-bind (transactions)
      (map-fn '(or transaction null)
	      (transactions-iterator object entry-transform))
    (until-if #'null transactions)))

(provide 'ledger)

;; ledger.lisp ends here
