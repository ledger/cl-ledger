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

(defun calculate-totals (binder &key (amount nil) (total nil))
  (declare (type (or string function null) amount))
  (declare (type (or string function null) total))
  (if (stringp amount) (setf amount (parse-value-expr amount)))
  (if (stringp total)  (setf total (parse-value-expr total)))
  (let ((running-total (make-instance 'balance)))
    (loop
       with iterator = (transactions-iterator binder)
       for xact = (funcall iterator)
       while xact do
       (let ((amt (if amount
		      (funcall amount xact)
		      (xact-amount xact))))
	 (if amt
	     (progn
	       (unless (eq amt (xact-amount xact))
		 (xact-set-value :display-amount amt))
	       (add* running-total amt))
	     (error "Running `calculate-running-totals' on non-normalized data"))
	 (xact-set-value xact :running-total
			 (copy-from-balance running-total)))))
  binder)

(export 'calculate-totals)

(provide 'totals)
