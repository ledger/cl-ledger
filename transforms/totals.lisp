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

(defun get-computed-amount (xact amount lots lot-prices lot-dates lot-tags)
  (strip-annotations (etypecase amount
		       (function (funcall amount xact))
		       (value-expr (value-expr-call amount xact))
		       (null (xact-amount xact)))
		     :keep-price (or lots lot-prices)
		     :keep-date  (or lots lot-dates)
		     :keep-tag   (or lots lot-tags)))

(defun get-computed-total (item total)
  (etypecase total
    (function (funcall total item))
    (value-expr (value-expr-call total item))))

(defun calculate-totals (xact-series &key
			 (amount nil) (display-amount nil)
			 (total nil) (display-total nil)
			 (lots nil)
			 (lot-prices nil) (lot-dates nil) (lot-tags nil)
			 &allow-other-keys)
  (declare (type series xact-series))

  (declare (type (or null value value-expr string function) amount))
  (declare (type (or null value value-expr string function) total))
  (declare (type (or null value value-expr string function) display-amount))
  (declare (type (or null value value-expr string function) display-total))

  (declare (type boolean lots))
  (declare (type boolean lot-prices))
  (declare (type boolean lot-dates))
  (declare (type boolean lot-tags))

  (if (and amount (stringp amount))
      (setf amount (parse-value-expr amount)))
  (if (and total (stringp total))
      (setf total (parse-value-expr total)))

  (if (and display-amount (stringp display-amount))
      (setf display-amount (parse-value-expr display-amount)))
  (if (and display-total (stringp display-total))
      (setf display-total (parse-value-expr display-total)))

  (let ((running-total 0)
	(*value-expr-series-offset* 0))
    (map-fn
     'transaction
     #'(lambda (xact)
	 (incf *value-expr-series-offset*)

	 (let ((amt (get-computed-amount xact amount lots
					 lot-prices lot-dates lot-tags)))
	   (setf (xact-value xact :computed-amount) amt
		 (xact-value xact :running-total)
		 (setf running-total (add running-total amt))))

	 (if total
	     ;; This function might well refer to the :running-total we just
	     ;; set.
	     (setf (xact-value xact :running-total)
		   (get-computed-total xact total)))

	 (if display-amount
	     (setf (xact-value xact :computed-amount)
		   (get-computed-amount xact display-amount lots
					lot-prices lot-dates lot-tags)))
	 (if display-total
	     (setf (xact-value xact :running-total)
		   (get-computed-total xact display-total)))
	 xact)
     xact-series)))

;; jww (2007-12-09): How do I get :lot-prices into this function?
(defun calculate-account-totals (xact-series &key (amount nil) (total nil)
				 (lots nil) (lot-prices nil) (lot-dates nil)
				 (lot-tags nil))
  (let (root-account)
    (iterate ((xact xact-series))
      (unless root-account
	(let ((binder (journal-binder (entry-journal (xact-entry xact)))))
	  (reset-accounts binder)
	  (setf root-account (binder-root-account binder))))

      (let* ((account (xact-account xact))
	     (subtotal (account-value account :subtotal))
	     (amt (get-computed-amount xact amount lots
				       lot-prices lot-dates lot-tags)))
	(setf (account-value account :subtotal)
	      (if subtotal (add subtotal amt) amt))))
    (labels
	((calc-accounts (account)
	   (let* ((subtotal (account-value account :subtotal))
		  (account-total (or subtotal 0)))

	     (let ((children (account-children account))
		   (children-with-totals 0))
	       (when children
		 (maphash #'(lambda (name account)
			      (declare (ignore name))
			      (let ((child-total (calc-accounts account)))
				(unless (value-zerop child-total)
				  (setf account-total
					(add account-total child-total))
				  (incf children-with-totals))))
			  children))
	       (setf (account-value account :children-with-totals)
		     children-with-totals))

	     (setf (account-value account :total) account-total)
	     (if total
		 ;; This function might well refer to the :total we just set.
		 (setf (account-value account :total)
		       (get-computed-total account total)))
	     (account-value account :total))))

      (if root-account
	  (calc-accounts root-account)))

    root-account))

(provide 'totals)
