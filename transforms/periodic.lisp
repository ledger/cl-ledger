;; periodic.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun group-by-period (xact-series period)
  (with-temporary-journal (journal)
    (iterate (((begin end xacts)
	       (collate-by-time-period xact-series period
				       :key #'xact-date)))
      (let* ((entry (make-instance 'entry
				   :journal journal
				   :actual-date begin
				   :payee (format nil "- ~A"
						  (strftime end))
				   :normalizedp t))
	     (account-hash (make-hash-table :test #'eq)))
	(add-to-contents journal entry)

	(iterate ((xact xacts))
	  (let* ((acct (xact-account xact))
		 (acct-xact (gethash acct account-hash)))
	    (unless acct-xact
	      (setf acct-xact
		    (make-transaction
		     :entry entry
		     :account (find-account journal (account-fullname acct)
					    :create-if-not-exists-p t)
		     :amount (balance))
		    (gethash acct account-hash)
		    acct-xact)
	      (add-transaction entry acct-xact))
	    (add* (xact-amount acct-xact) (xact-amount xact))))))

    (scan-transactions journal)))

(defun periodic-transform (xact-series period)
  ;; jww (2007-12-01): this is a temporary hack
  (let ((range (parse-time-period period)))
    (setf period (time-period :range range
			      :step (time-range-duration range)))
    (group-by-period xact-series (if (stringp period)
				     (parse-time-period period)
				     period))))

(provide 'periodic)

;; periodic.lisp ends here
