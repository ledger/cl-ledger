;; subtotal.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun group-by-account (xact-series)
  (with-temporary-journal (journal)
    (let (earliest-date latest-date)
      (iterate ((xact xact-series))
	(let* ((account (find-account journal
				      (account-fullname (xact-account xact))
				      :create-if-not-exists-p t))
	       (balance (or (account-value account :subtotal) 0)))

	  ;; Remember the earliest and latest transactions in the set, so that
	  ;; we can create a meaningful display name for the grouped entry.
	  (if (or (null earliest-date)
		  (timestamp< (xact-date xact) earliest-date))
	      (setf earliest-date (xact-date xact)))
	  (if (or (null latest-date)
		  (timestamp> (xact-date xact) latest-date))
	      (setf latest-date (xact-date xact)))

	  (account-set-value account
			     :subtotal (add balance (xact-amount xact)))))

      (if latest-date
	  (let ((totals-entry
		 (make-instance 'entry
				:journal journal
				:actual-date earliest-date
				:payee (format nil "- ~A"
					       (strftime latest-date))
				:normalizedp t)))
	    (labels
		((walk-account (account)
		   (if-let ((subtotal (account-value account :subtotal)))
		     (add-transaction totals-entry
				      (make-transaction :entry   totals-entry
							:account account
							:amount  subtotal)))
		   (if-let ((children (account-children account)))
		     (maphash #'(lambda (name child)
				  (declare (ignore name))
				  (walk-account child))
			      children))))
	      (walk-account (binder-root-account (journal-binder journal))))
	    (scan-transactions totals-entry))
	  (scan '())))))

(provide 'subtotal)

;; subtotal.lisp ends here
