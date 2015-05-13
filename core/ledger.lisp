;; ledger.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(require 'types)

;;;_ * Global variables

(defvar *use-effective-dates* nil)
(defvar *registered-parsers* nil)
(defvar *allow-embedded-lisp* nil)

;;;_ * Binders

(defvar *last-binder* nil)

(defun compare-path-lists (left right)
  (when (= (length left) (length right))
    (dolist (l left)
      (dolist (r right)
	(unless (equal l r)
	  (return-from compare-path-lists nil))))
    t))

(defun binder (&rest args)
  (let (binder objects)
    (loop while args do
	 (etypecase (first args)
	   (keyword (loop-finish))
	   (binder
	    (setf binder (first args))
	    (dolist (journal (binder-journals binder))
	      (push journal objects)))
	   ((or string pathname journal)
	    (push (first args) objects)))
	 (setf args (cdr args)))

    (if (null objects)
	(return-from binder (values *last-binder* args)))

    (if binder
	(setf *last-binder* nil)
	(setf binder *last-binder*))

    (unless (and *last-binder*
		 (compare-path-lists
		  (mapcar #'(lambda (obj)
			      (etypecase obj
				(journal (journal-source obj))
				(pathname obj)
				(string (pathname obj))))
			  objects)
		  (mapcar #'journal-source
			  (binder-journals *last-binder*))))
      (setf binder (make-instance 'binder))
      (dolist (object objects)
	(add-journal binder object)))

    (when (and binder (binderp binder))
      (setf *last-binder* binder)

      (loop for journal-cell on (binder-journals binder) do
	   (when (or (null (journal-read-date (car journal-cell)))
		     (> (file-write-date (journal-source (car journal-cell)))
			(journal-read-date (car journal-cell))))
	     (setf (car journal-cell)
		   (read-journal (journal-source
				  (car journal-cell)) binder))
	     (assert (car journal-cell))))

      (values binder args))))

(defun binder-time-range (&optional (binder *last-binder*))
  (with-timestamp-range (earliest latest)
    (iterate ((xact (scan-transactions binder)))
      (update-range (xact-date xact)))
    (time-range :begin earliest :end latest :end-inclusive-p t)))

(defun binder-statistics (&optional (binder *last-binder*))
  (format t "~&Binder statistics:~%")
  (let ((range (binder-time-range binder)))
    (format t "~12@A earliest date~%" (strftime (time-range-begin range)))
    (format t "~12@A latest date~%" (strftime (time-range-end range)))
    (format t "~12D days covered (approx.)~%"
	    (round (duration-seconds (time-range-duration range))
		   86400)))
  (format t "~12D transactions~%"
	  (collect-length (scan-transactions binder)))
  (format t "~12D entries~%" (collect-length (scan-entries binder)))
  (format t "~12D unique payees~%"
	  (length
	   (remove-duplicates
	    (collect (map-fn 'string #'entry-payee (scan-entries binder)))
	    :test #'string=)))
  (format t "~12D accounts~%"
	  (length
	   (remove-duplicates
	    (collect (map-fn 'account #'xact-account
			     (scan-transactions binder)))
	    :test #'eq)))
  (format t "~12D commodities (including annotated)~%"
	  (hash-table-count
	   (cambl::commodity-pool-by-name-map *default-commodity-pool*)))
  (format t "~12D base commodities~%"
	  (length
	   (remove-duplicates
	    (mapcar #'(lambda (commodity)
			(if (annotated-commodity-p commodity)
			    (cambl::get-referent commodity)
			    commodity))
		    (let (lst)
		      (maphash #'(lambda (k v) (declare (ignore k)) (push v lst))
			       (cambl::commodity-pool-by-name-map
				*default-commodity-pool*))
		      lst))))))

;;;_ * Journals

(defmacro with-temporary-journal ((var) &body body)
  "Creates a wrapper around BODY which binds VAR to a temporary journal."
  (let ((artificial-binder-sym (gensym)))
    `(let ((,artificial-binder-sym (make-instance 'binder))
	   (,var (make-instance 'journal)))
       (add-journal ,artificial-binder-sym ,var)
       ,@body)))

(defun read-journal (path &optional binder)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (unless binder
    (setf binder (or *last-binder* (make-instance 'binder))
	  *last-binder* binder))
  (if (stringp path)
      (setf path (pathname path)))
  (with-open-file (in path :direction :input)
    (let ((start-position (file-position in)))
      (dolist (parser *registered-parsers*)
	(if-let ((journal (funcall parser in binder)))
	  (setf (journal-read-date journal)
		(file-write-date path)
		(journal-source journal) path)
	  (return-from read-journal journal)))
      (file-position in start-position)
      nil)))

(defun add-journal-from-path (binder path)
  (if-let ((journal (read-journal path binder)))
    (add-journal binder journal)))

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
	(progn
	  (rplacd value-cell value)
	  (values (cdr value-cell) value-cell))
	(progn
	  (push (cons key value) (account-data account))
	  (values value (first (account-data account)))))))

(defsetf account-value (xact key) (value)
  (let ((xact-sym (gensym))
	(key-sym (gensym)))
    `(let* ((,xact-sym ,xact)
	    (,key-sym ,key)
	    (value-cell (assoc ,key-sym (account-data ,xact-sym))))
       (if value-cell
	   (rplacd value-cell ,value)
	   (push (cons ,key-sym ,value)
		 (account-data ,xact-sym))))))

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
		       account-path)))

(defmethod find-account ((journal journal) (account-path string)
			 &key (create-if-not-exists-p nil))
  (find-account (journal-binder journal) account-path
		:create-if-not-exists-p create-if-not-exists-p))

;;;_ * Entries

(defun copy-entry (entry &rest args)
  (apply #'make-instance 'entry
	 :actual-date    (entry-actual-date entry)
	 :effective-date (entry-effective-date entry)
	 :status	 (entry-status entry)
	 :code		 (entry-code entry)
	 :payee		 (entry-payee entry)
	 :note		 (entry-note entry)
	 args))

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
      (loop while contents-iterators do
	   (let ((item (funcall (first contents-iterators))))
	     (if (null item)
		 (pop contents-iterators)
		 (progn
		   (if (eq (class-of item) entry-class)
		       (return item))
		   (if (typep item 'journal)
		       (push (list-iterator (journal-contents item))
			     contents-iterators)))))))))

(defmethod entries-iterator ((entry entry))
  (list-iterator (list entry)))

(defun entries-list (object)
  (loop with iterator = (entries-iterator object)
     for entry = (funcall iterator) while entry collect entry))

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
      (do-recurse next-xact ()
	(or (funcall xacts-iterator)
	    (if-let ((next-journal (funcall journals-iterator)))
	      (setf xacts-iterator
		    (transactions-iterator next-journal
					   entry-transform))
	      (next-xact)))))))

(defmethod transactions-iterator ((journal journal) &optional entry-transform)
  (let ((entries-iterator (entries-iterator journal))
	(xacts-iterator (constantly nil)))
    (lambda ()
      (do-recurse next-xact ()
	(or (funcall xacts-iterator)
	    (if-let ((next-entry (funcall entries-iterator)))
	      (setf xacts-iterator
		    (transactions-iterator next-entry
					   entry-transform))
	      (next-xact)))))))

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
  (loop with iterator = (transactions-iterator object entry-transform)
     for xact = (funcall iterator) while xact collect xact))

(defmacro map-transactions (callable object &key (entry-transform nil))
  `(map-iterator ,callable (transactions-iterator ,object ,entry-transform)))

;; jww (2007-11-19): deprecated?
(defmacro do-transactions ((var object &optional (result nil)) &body body)
  (let ((iterator (gensym)))
    `(loop with ,iterator = (transactions-iterator ,object)
	for ,var = (funcall ,iterator) while ,var do
	  (progn ,@body ,result))))

(declaim (inline scan-transactions))
(defun scan-transactions (object &optional entry-transform)
  (declare (optimizable-series-function))
  (multiple-value-bind (transactions)
      (map-fn '(or transaction null)
	      (transactions-iterator object entry-transform))
    (until-if #'null transactions)))

(provide 'ledger)

;; ledger.lisp ends here
