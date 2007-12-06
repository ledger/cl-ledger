;; This is a Ledger transform whose sole job is to walk through a binder and
;; calculate running totals for all the transactions in it, in order.  The
;; total is stored in the `xact-data' element of each transaction, just lookup
;; the :running-total key in that alist.

;; This is a good example of an Annotate Transform, which does not modify the
;; structure of the binder or its primary data fields, but merely annotates
;; the `data' field of transactions.  If a running total had been performed
;; previously, it's results are completely overwritten.

(declaim (optimize (safety 3) (debug 3)))

(in-package :ledger)

(defun calculate-totals (xact-series &key (amount nil) (total nil))
  (declare (type series xact-series))
  (let ((running-total (cambl:balance))
	(*value-expr-series-offset* 0))
    (map-fn
     'transaction
     #'(lambda (xact)
	 (incf *value-expr-series-offset*)

	 (let ((amt (typecase amount
		      (function (funcall amount xact))
		      (value-expr (value-expr-call amount xact))
		      (otherwise (xact-amount xact)))))

	   (if (balance-p amt)
	       (setf running-total (add running-total amt))
	       (let* ((commodity (etypecase amt
				   (amount (amount-commodity amt))
				   (integer nil)))
		      (current (assoc commodity
				      (get-amounts-map running-total))))

		 ;; This custom version of add* cuts down on cons'ing by a
		 ;; factor of 10.  We can do this because we it's OK for all
		 ;; of the running totals to share the same balance structure.
		 (if current
		     (setf (cdr current) (add (cdr current) amt))
		     (progn
		       (setf running-total
			     (shallow-copy-balance running-total))

		       (push (cons commodity amt)
			     (get-amounts-map running-total))

		       (when (> (length (get-amounts-map running-total)) 1)
			 (setf (get-amounts-map running-total)
			       (sort (get-amounts-map running-total)
				     #'commodity-lessp :key #'car)))))))

	   (setf (xact-value xact :running-total) running-total)

	   (if total
	       (setf (xact-value xact :running-total)
		     (etypecase total
		       (function (funcall total xact))
		       (value-expr (value-expr-call total xact))))))
	 xact)
     xact-series)))

(defun calculate-account-totals (xact-series)
  (let (root-account)
    (iterate ((xact xact-series))
      ;; After the first transaction, reset the binder since we're going to
      ;; store temporary data that might exist from a previous calculation.
      (unless root-account
	(let ((binder (journal-binder (entry-journal (xact-entry xact)))))
	  (reset-accounts binder)
	  (setf root-account (binder-root-account binder))))

      (let* ((account (xact-account xact))
	     (balance (account-value account :subtotal)))
	(if balance
	    (add* balance (xact-amount xact))
	    (account-set-value account
			       :subtotal (balance (xact-amount xact))))))

    (labels
	((calc-accounts (account)
	   (let* ((subtotal (account-value account :subtotal))
		  (total (if subtotal
			     (copy-balance subtotal)
			     (balance))))

	     (account-set-value account :total total)

	     (let ((children (account-children account))
		   (children-with-totals 0))
	       (when children
		 (maphash #'(lambda (name account)
			      (declare (ignore name))
			      (let ((child-total (calc-accounts account)))
				(add* total child-total)
				(unless (value-zerop child-total)
				  (incf children-with-totals))))
			  children))

	       (account-set-value account
				  :children-with-totals children-with-totals))
	     
	     total)))

      (if root-account
	  (calc-accounts root-account)))

    root-account))

(provide 'totals)
