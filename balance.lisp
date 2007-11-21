;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun print-balance (xact-series &key (output-stream *standard-output*))
  (let ((count 0)
	root-account)
    (iterate
     ((xact xact-series))

     (unless root-account
       (let ((binder (journal-binder (entry-journal (xact-entry xact)))))
	 (reset-binder binder)
	 (setf root-account (binder-root-account binder))))

     (add-transaction (xact-account xact) xact)
     (incf count))

    (labels
	((calc-accounts (account)
	   (let* ((subtotal
		   (collect-fn 'cambl:balance #'cambl:balance
			       #'(lambda (bal xact)
				   (add* bal (xact-resolve-amount xact)))
			       (scan-transactions account)))
		  (total (copy-balance subtotal))
		  (children-with-totals 0))

	     (account-set-value account :subtotal subtotal)
	     (account-set-value account :total    total)

	     (let ((children (account-children account)))
	       (when children
		 (maphash #'(lambda (name account)
			      (declare (ignore name))
			      (let ((child-total (calc-accounts account)))
				(add* total child-total)
				(unless (value-zerop child-total)
				  (incf children-with-totals))))
			  children))

	       ;; Don't print this account if it's identical to the single
	       ;; child.  In this way, the parent is "elided" out of the
	       ;; printing process.
	       (account-set-value account :displayp
				  (not (and (value-zerop subtotal)
					    (<= children-with-totals 1)))))
	     
	     total))

	 (get-partial-name (string account count)
	   (if (zerop count)
	       string
	       (progn
		 (setf account (account-parent account))
		 (get-partial-name
		  (concatenate 'string (account-name account)
			       ":" string)
		  account (1- count)))))

	 (print-accounts (account name elided real-depth)
	   (if (plusp real-depth)
	       (if (account-value account :displayp)
		   (format
		    output-stream "~12A  ~vA~A~%"
		    (format-value (account-value account :total)
				  :width 12)
		    (* (- (1- real-depth) elided) 2) ""
		    (get-partial-name name account elided))
		   (incf elided)))

	   (if (account-children account)
	       (locally (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
		 (mapc #'(lambda (cell)
			   (print-accounts (cdr cell) (car cell)
					   elided (1+ real-depth)))
		       (sort (let (lst)
			       (maphash #'(lambda (key value)
					    (push (cons key value) lst))
					(account-children account))
			       lst)
			     #'string< :key #'car))))))

      (calc-accounts root-account)
      (print-accounts root-account "" 0 0)

      (format output-stream
	      "-----------------------------------------------------~%~12A~%"
	      (format-value (account-value root-account :total) :width 12)))
    count))

(defun balance-report (binder &rest args)
  "This is a convenience function for quickly making register reports.

  A typical usage might be:

    (ledger:balance-report \"/path/to/ledger.dat\"
                           :begin \"2007/08/26\" :account \"food\")"
  (let ((binder (etypecase binder
		  ((or string pathname) (read-binder binder))
		  (binder binder)))
	(transforms (determine-transforms args)))

    (let ((xacts (scan-normalized-transactions binder)))
      (dolist (transform transforms)
	(setf xacts (apply (car transform) xacts (cdr transform))))
      (print-balance xacts))))

(provide 'register)

;; register.lisp ends here
