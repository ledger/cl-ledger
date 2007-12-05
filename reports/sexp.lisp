;; sexp.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun transactions-to-sexp (xact-series &key (no-total nil)
			     &allow-other-keys)
  (mapcar
   #'(lambda (entry-xacts)
       (let ((entry (xact-entry (first entry-xacts))))
	 (list (item-position-begin-line (entry-position entry))
	       (append (multiple-value-list
			(floor (unix-time (entry-date entry))
			       65536))
		       (list 0))
	       (entry-status entry)
	       (entry-payee entry)
	       (mapcar
		#'(lambda (xact)
		    (list (and (xact-position xact)
			       (item-position-begin-line (xact-position xact)))
			  (xact-status xact)
			  (account-fullname (xact-account xact))
			  (format-value (xact-amount xact))
			  (if no-total ""
			      (format-value
			       (or (xact-value xact :running-total) 0)))))
		entry-xacts))))
   (group-transactions-by-entry (collect xact-series))))

(defun sexp-report (&rest args)
  (basic-reporter #'transactions-to-sexp args))

(defun find-unique-payees (&rest args)
  (remove-duplicates
   (collect (map-fn 'string #'entry-payee
		    (scan-entries (get-related-binder args))))
   :test #'string=))

(defun find-account-tree (&rest args)
  (let* ((binder (get-related-binder args))
	 (root-account (binder-root-account binder)))
    (labels
	((find-accounts (account)
	   (cons
	    (account-name account)
	    (let ((children (account-children account))
		  child-accounts)
	      (when children
		(maphash #'(lambda (name account)
			     (declare (ignore name))
			     (push (find-accounts account)
				   child-accounts))
			 children))
	      child-accounts))))
      (cdr (find-accounts root-account)))))

(provide 'sexp)

;; sexp.lisp ends here
