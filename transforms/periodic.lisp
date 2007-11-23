;; periodic.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun group-by-period (xact-series period)
  (let ((artificial-binder (make-instance 'binder))
	(artificial-journal (make-instance 'journal)))
    (add-journal artificial-binder artificial-journal)
    (iterate
     (((begin end xacts)
       (collate-by-time-period xact-series period :key #'xact-date)))
     (let* ((entry
	     (make-instance 'entry
			    :journal artificial-journal
			    :actual-date begin
			    :payee (format nil "- ~A" (strftime end))))
	    (account-hash (make-hash-table :test #'eq)))
       (add-to-contents artificial-journal entry)
       (iterate
	((xact xacts))
	(let* ((acct (xact-account xact))
	       (acct-xact (gethash acct account-hash)))
	  (unless acct-xact
	    (setf acct-xact
		  (make-transaction :entry entry
				    :account acct
				    :amount (balance))
		  (gethash acct account-hash)
		  acct-xact)
	    (add-transaction entry acct-xact))
	  (add* (xact-amount acct-xact)
		(xact-resolve-amount xact))))))
    (scan-transactions artificial-journal)))

(defun periodic-transform (xact-series &rest args)
  (let ((period (getf args :period)))
    (group-by-period xact-series (if (stringp period)
				     nil
				     ;; jww (2007-11-21): (parse-time-period period)
				     period))))

(provide 'periodic)

;; periodic.lisp ends here
