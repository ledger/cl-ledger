;; filter.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun destructively-filter (binder &rest args)
  (let (filter)

    ;; "Cook" the criteria into callable functions
    (flet ((cook-regex (pattern)
	     (cl-ppcre:create-scanner pattern :case-insensitive-mode t))
	   (cook-date (datestring)
	     (cambl:parse-datetime datestring)))
      (setf
       filter
       (compile
	nil
	`(lambda (xact)
	   (let ((entry (xact-entry xact)))
	     (declare (ignorable entry))
	     (and
	      ,@(loop
		   for arg = args then (rest (rest arg))
		   while arg
		   for keyword = (first arg)
		   for value   = (first (rest arg))
		   collect
		   (case keyword
		     ((:account :not-account)
		      (assert (or (stringp value)
				  (typep value 'account)))
		      (if (stringp value)
			  (let ((form `(cl-ppcre:scan
					,(cook-regex value)
					(account-fullname (xact-account xact)))))
			    (if (eq keyword :account)
				form
				`(not ,form)))
			  `(eq ,value (xact-account xact))))
		     ((:payee :not-payee)
		      (assert (stringp value))
		      (let ((form `(cl-ppcre:scan ,(cook-regex value)
						  (entry-payee entry))))
			(if (eq keyword :payee)
			    form
			    `(not ,form))))
		     ((:note :not-note)
		      (assert (stringp value))
		      (let* ((scanner (cook-regex value))
			     (form
			      `(or (and (xact-note xact)
					(cl-ppcre:scan ,scanner
						       (xact-note xact)))
				   (and (entry-note entry)
					(cl-ppcre:scan ,scanner
						       (entry-note entry))))))
			(if (eq keyword :note)
			    form
			    `(not ,form)))) 
		     (:expr
		      (assert (stringp value))
		      `(funcall ,(parse-value-expr value) xact))
		     ((:begin :end)
		      (assert (or (stringp value)
				  (typep value 'cambl:datetime)))
		      `(,(if (eq keyword :begin)
			     'local-time>=
			     'local-time<=)
			 (xact-date xact) ,(if (stringp value)
					       (cook-date value)
					       value)))))))))))

    (dolist (journal (binder-journals binder))
      (setf (journal-entries journal)
	    (loop
	       for entry in (journal-entries journal)
	       do (setf (entry-transactions entry)
			(loop
			   for xact in (entry-transactions entry)
			   when (funcall filter xact)
			   collect xact))
	       when (plusp (length (entry-transactions entry)))
	       collect entry)))

    (labels
	((filter-in-account (name account)
	   (declare (ignore name))
	   (setf (account-transactions account)
		 (loop
		    for xact in (account-transactions account)
		    when (funcall filter xact)
		    collect xact))
	   (let ((children (account-children account)))
	     (if children
		 (maphash #'filter-in-account children)))))
      (filter-in-account "" (binder-root-account binder)))

    (if (binder-transactions binder)
	(setf (binder-transactions binder)
	      (loop
		 for xact in (binder-transactions binder)
		 when (funcall filter xact)
		 collect xact))))
  binder)

(export 'destructively-filter)

(provide 'filter)

;; filter.lisp ends here
