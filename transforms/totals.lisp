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
  (declare (type (or string function null) amount))
  (declare (type (or string function null) total))
  (if (stringp amount) (setf amount (parse-value-expr amount)))
  (if (stringp total)  (setf total (parse-value-expr total)))
  (let ((running-total (cambl:balance)))
    (map-fn
     'transaction
     #'(lambda (xact)
	 (let ((amt (if amount
			(funcall amount xact)
			(xact-amount xact))))
	   (setf (xact-value xact :running-total)
		 (copy-from-balance (add* running-total amt)))
	   (if total
	       (setf (xact-value xact :running-total)
		     (funcall total xact))))
	 xact)
     xact-series)))

(defun calculate-account-totals (xact-series)
  (let (root-account)
    (iterate
     ((xact xact-series))

     ;; After the first transaction, reset the binder since we're going to
     ;; store temporary data that might exist from a previous calculation.
     (unless root-account
       (let ((binder (journal-binder (entry-journal (xact-entry xact)))))
	 (reset-accounts binder)
	 (setf root-account (binder-root-account binder))))

     (add-transaction (xact-account xact) xact))

    (labels
	((calc-accounts (account)
	   (let* ((subtotal
		   (collect-fn 'cambl:balance #'cambl:balance
			       #'(lambda (bal xact)
				   (add* bal (xact-amount xact)))
			       (scan-transactions account)))
		  (total (copy-balance subtotal)))

	     (account-set-value account :subtotal subtotal)
	     (account-set-value account :total    total)

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

      (calc-accounts root-account))

    root-account))

(provide 'totals)
