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
	       (case (entry-status entry)
		 (:cleared "*")
		 (:uncleared "")
		 (:pending "!"))
	       (entry-payee entry)
	       (mapcar
		#'(lambda (xact)
		    (list (item-position-begin-line (xact-position xact))
			  (case (xact-status xact)
			    (:cleared "*")
			    (:uncleared "")
			    (:pending "!"))
			  (account-fullname (xact-account xact))
			  (format-value (xact-amount xact))
			  (if no-total ""
			      (format-value
			       (or (xact-value xact :running-total) 0)))))
		entry-xacts))))
   (group-transactions-by-entry (collect xact-series))))

(defun sexp-report (&rest args)
  (basic-reporter #'transactions-to-sexp args))

(provide 'sexp)

;; sexp.lisp ends here
