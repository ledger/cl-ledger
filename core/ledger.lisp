;; ledger.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(require 'types)

(defun print-transaction (transaction stream depth)
  (declare (ignore depth))
  (print-unreadable-object (transaction stream :type t)
    (format stream
	    ":DATE ~S :ACCT ~S :AMT ~S :V ~S :M ~S :G ~S :C ~S :POS ~S"
	    (let ((date (xact-date transaction)))
	      (and date (strftime date)))
	    (account-fullname (xact-account transaction))
	    (let ((amt (xact-amount transaction)))
	      (and (typep amt 'value)
		   (format-value amt)))
	    (xact-virtualp transaction)
	    (xact-must-balance-p transaction)
	    (xact-generatedp transaction)
	    (xact-calculatedp transaction)
	    (let ((pos (xact-position transaction)))
	      (and pos (item-position-begin-char pos))))))

(declaim (inline xact-value))
(defun xact-value (xact key)
  (let ((value-cell (assoc key (xact-data xact))))
    (values (cdr value-cell) value-cell)))

(declaim (inline xact-value))
(defun xact-set-value (xact key value)
  (let ((value-cell (assoc key (xact-data xact))))
    (if value-cell
	(rplacd value-cell value)
	(push (cons key value) (xact-data xact)))))

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

;; Textual journal parser

(defvar *use-effective-dates* nil)
(defvar *registered-parsers* nil)
(defvar *allow-embedded-lisp* nil)

;;;_ * Code for construction of the LEDGER object tree

(defun add-journal-file (binder path)
  "Read in a textual Ledger journal from the given PATH.
The result is of type JOURNAL."
  (with-open-file (in path :direction :input)
    (let ((start-position (file-position in)))
      (dolist (parser *registered-parsers*)
	(let ((journal (funcall parser in binder)))
	  (if journal
	      (return-from nil journal)
	      (file-position in start-position)))))))

(defun binder (path-or-string)
  (let ((binder
	 (make-instance 'binder :commodity-pool *default-commodity-pool*)))
    (if (typep path-or-string '(or pathname string))
	(let ((journal (add-journal-file binder path-or-string)))
	  (if journal
	      (add-journal binder journal)))
	(error "unknown"))
    binder))

(declaim (inline read-binder))
(defun read-binder (path-or-string)
  (binder path-or-string))

(defmethod add-journal ((binder binder) (journal journal))
  (pushend journal (binder-journals binder)))

(defmethod add-journal ((journal journal) (child journal))
  (pushend child (journal-contents journal)))

(defun reset-binder (binder)
  (setf (binder-transactions binder) nil)
  (labels ((undo-filter-in-account (name account)
	     (declare (ignore name))
	     (setf (account-data account) nil
		   (account-transactions account) nil
		   (account-last-transaction-cell account) nil)
	     (let ((children (account-children account)))
	       (if children
		   (maphash #'undo-filter-in-account children)))))
    (undo-filter-in-account "" (binder-root-account binder)))
  binder)

(declaim (inline add-to-contents))
(defun add-to-contents (journal item)
  (declare (type journal journal))
  (pushend item (journal-contents journal)
	   (journal-last-content-cell journal)))

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

(defmethod add-transaction ((entry entry) (transaction transaction))
  (pushend transaction (entry-transactions entry)))

(defmethod add-transaction ((account account) (transaction transaction))
  (pushend transaction (account-transactions account)
	   (account-last-transaction-cell account)))

;;;_ * Code to access and change object details

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

(defun xact-resolve-amount (xact)
  (declare (type transaction xact))
  (or (xact-value xact :computed-amount)
      (let ((amount (xact-amount xact)))
	(cond
	  ((valuep amount)
	   amount)
	  ((null amount)
	   (error "Resolving transaction amount for unnormalized data"))
	  ((functionp amount)
	   (xact-set-value xact :computed-amount (funcall amount xact)))
	  (t
	   (error "impossible"))))))

(declaim (inline xact-cleared-p))
(defun xact-cleared-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) 'cleared))

(declaim (inline xact-pending-p))
(defun xact-pending-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) 'pending))

(declaim (inline xact-uncleared-p))
(defun xact-uncleared-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) 'uncleared))

(declaim (inline parse-journal-date))
(defun parse-journal-date (journal string)
  (strptime string :format (or (journal-date-format journal)
			       *input-time-format*)
	    :default-year (journal-default-year journal)))

;;;_ * General utility functions

(defmacro while (test-form &body body)
  `(do () ((not ,test-form))
     ,@body))

(declaim (inline list-iterator))
(defun list-iterator (list)
  (lambda ()
    (prog1
	(first list)
      (setf list (rest list)))))

(declaim (inline ignore-args))
(defun ignore-args (closure)
  (lambda (&rest args)
    (declare (ignore args))
    (funcall closure)))

(declaim (inline map-iterator))
(defun map-iterator (callable iterator)
  ;; This makes the assumption that iterator returns NIL when it reaches end
  ;; of series.
  (loop
     for value = (funcall iterator)
     while value do (funcall callable value)))

(defmacro do-iterator ((var iterator &optional (result nil)) &body body)
  `(block nil
     (map-iterator #'(lambda (,var) ,@body) ,iterator)
     ,result))

(defun chain-functions (first-arg &rest args)
  "Call a group of functions by chaining, passing all keyword args.

  This function allows you to call a set of functions like this:

    (chain-functions arg #'foo :foo 10 :foo2 20
                         #'bar :bar 30)

  This is equivalent to:

    (bar (foo arg :foo 10 :foo2 20) :bar 30)"
  (if args
      (apply
       #'chain-functions
       (apply (first args)
	      (cons first-arg
		    (when (rest args)
		      (setf args (rest args))
		      (loop
			 while (keywordp (first args))
			 collect (first args)
			 collect (first (rest args))
			 do (setf args (rest (rest args)))))))
       args)
      first-arg))

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

(defmethod transactions-iterator ((binder binder) &optional entry-transform)
  (let ((xacts (binder-transactions binder)))
    (if xacts
	(list-iterator xacts)
	(let ((journals-iterator
	       (list-iterator (binder-journals binder)))
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
	      (next-xact)))))))

;; jww (2007-11-19): implement
(defmethod transactions-iterator ((account account) &optional entry-transform)
  (declare (ignore entry-transform))
  (list-iterator (account-transactions account)))

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

(declaim (inline scan-transactions))
(defun scan-normalized-transactions (object)
  (declare (optimizable-series-function))
  (scan-transactions object #'normalize-entry))

(provide 'ledger)

;; ledger.lisp ends here
