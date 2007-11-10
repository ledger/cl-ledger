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

(defun calculate-totals (binder)
  (let ((running-total (integer-to-amount 0)))
    (dolist (journal (binder-journals binder))
      (dolist (entry (journal-entries journal))
	(dolist (xact (entry-transactions entry))
	  (let ((amt (xact-amount xact)))
	    (if amt
		(setq running-total (add running-total amt))
		(error "Running `calculate-running-totals' on non-normalized data"))
	    (let ((total (assoc :running-total (xact-data xact))))
	      (if total
		  (rplacd total running-total)
		  (push (cons :running-total running-total)
			(xact-data xact)))))))))
  binder)

(export 'calculate-totals)
