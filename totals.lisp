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
  (let ((running-total (balance)))
    (map-fn
     '(values transaction value value)
     #'(lambda (xact)
	 (let ((amt (xact-resolve-amount xact)))
	   (values xact amt (copy-from-balance (add* running-total amt)))))
     xact-series)))

(export 'calculate-totals)

(provide 'totals)
