;; balance.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun calculate-accounts (xact-series)
  (let (root-account)
    (iterate
     ((xact xact-series))

     ;; After the first transaction, reset the binder since we're going to
     ;; store temporary data that might exist from a previous calculation.
     (unless root-account
       (let ((binder (journal-binder (entry-journal (xact-entry xact)))))
	 (reset-binder binder)
	 (setf root-account (binder-root-account binder))))

     (add-transaction (xact-account xact) xact))

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
	     
	     total)))

      (calc-accounts root-account)
      root-account)))

(defun balance-reporter (&key (output-stream *standard-output*))
  (labels
      ((get-partial-name (string account count)
	 (if (zerop count)
	     string
	     (progn
	       (setf account (account-parent account))
	       (get-partial-name (concatenate 'string (account-name account)
					      ":" string)
				 account (1- count))))))
    (lambda (account real-depth elided &optional finalp)
      (if (not finalp)
	  (format output-stream "~12A  ~vA~A~%"
		  (format-value (account-value account :total) :width 12)
		  (* (- (1- real-depth) elided) 2) ""
		  (get-partial-name (account-name account) account elided))
	  (format output-stream
		  "-----------------------------------------------------~%~12A~%"
		  (format-value (account-value account :total) :width 12))))))

(defun print-balance (xact-series &key (reporter nil))
  (let ((root-account (calculate-accounts xact-series))
	(reporter (or reporter (balance-reporter))))
    (labels
	((print-accounts (account name elided real-depth)
	   (if (plusp real-depth)
	       (if (account-value account :displayp)
		   (funcall reporter account real-depth elided)
		   (incf elided)))

	   (if (account-children account)
	       (locally #+sbcl(declare (sb-ext:muffle-conditions
					sb-ext:code-deletion-note))
			(mapc #'(lambda (cell)
				  (print-accounts (cdr cell) (car cell)
						  elided (1+ real-depth)))
			      (sort (let (lst)
				      (maphash #'(lambda (key value)
						   (push (cons key value) lst))
					       (account-children account))
				      lst)
				    #'string< :key #'car))))))

      (print-accounts root-account "" 0 0)
      (funcall reporter root-account 0 0 t))))

(defun balance-report (&rest args)
  (basic-reporter #'print-balance args))

(provide 'balance)

;; balance.lisp ends here
