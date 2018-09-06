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

(defun get-computed-amount (xact &optional amount lots
			    lot-prices lot-dates lot-tags)
  (strip-annotations (etypecase amount
		       (function (funcall amount xact))
		       (value-expr (value-expr-call amount xact))
		       (null (xact-amount xact)))
		     :keep-price (or lots lot-prices)
		     :keep-date  (or lots lot-dates)
		     :keep-tag   (or lots lot-tags)))

(defun get-computed-total (item &optional total)
  (etypecase total
    (value total)
    (function (funcall total item))
    (value-expr (value-expr-call total item))
    (null
     (etypecase item
       (transaction (xact-value item :running-total))
       (account (account-value item :total))))))

(defun displayed-amount-setter (&key (amount nil) (total nil) (lots nil)
				(lot-prices nil) (lot-dates nil)
				(lot-tags nil) &allow-other-keys)
  (declare (type (or null value value-expr string function) amount))
  (declare (type (or null value value-expr string function) total))

  (declare (type boolean lots))
  (declare (type boolean lot-prices))
  (declare (type boolean lot-dates))
  (declare (type boolean lot-tags))

  (if (and amount (stringp amount))
      (setf amount (parse-value-expr amount)))
  (if (and total (stringp total))
      (setf total (parse-value-expr total)))

  (lambda (xact)
    (setf (xact-value xact :display-amount)
	  (get-computed-amount xact amount lots
			       lot-prices lot-dates lot-tags)
	  (xact-value xact :display-total)
	  (get-computed-total xact total))))

(defun calculate-totals (xact-series &key (set-amount nil) (set-total nil) 
			 (displayed-amount-setter nil) &allow-other-keys)
  (declare (type series xact-series))

  (declare (type (or null value value-expr string function) set-amount))
  (declare (type (or null value value-expr string function) set-total))

  (if (and set-amount (stringp set-amount))
      (setf set-amount (parse-value-expr set-amount)))
  (if (and set-total (stringp set-total))
      (setf set-total (parse-value-expr set-total)))

  (let ((running-total 0)
	(running-cost-total 0)
	(*value-expr-series-offset* 0)
	*value-expr-last-xact*)
    ;; Here the purpose of the "(scan (collect ..." is to force the computation
    ;; of the result to happen right now, while the temporary bindings for
    ;; *value-expr-series-offset* and *value-expr-last-xact* are active.
    (scan (collect
              (map-fn
               'transaction
               #'(lambda (xact)
	           (incf *value-expr-series-offset*)

	           (let ((amt (get-computed-amount xact set-amount)))
	             (setf (xact-value xact :computed-amount) amt
		           (xact-value xact :running-total)
		           (setf running-total (add running-total amt))
		           (xact-value xact :running-cost-total)
		           (setf running-cost-total
		                 (add running-cost-total
                                      (or (xact-cost xact) amt)))))

	           (if set-total
	               ;; This function might well refer to the :running-total
	               ;; we just set.
	               (setf (xact-value xact :running-total)
		             (get-computed-total xact set-total)))

	           (if displayed-amount-setter
	               (funcall displayed-amount-setter xact))

	           (setf *value-expr-last-xact* xact))
               xact-series)))))

(defun bridge-running-totals (xact-series &key (displayed-amount-setter nil)
			      (todays-total nil)
			      (revaluation-account "<Re-valuation>")
			      &allow-other-keys)
  (declare (type series xact-series))

  (if (and todays-total (stringp todays-total))
      (setf todays-total (parse-value-expr todays-total)))

  (scan
   (gathering ((xacts-out collect))
     (with-temporary-journal (journal)
       (let ((revaluation-account
	      (find-account journal revaluation-account
			    :create-if-not-exists-p t))
	     last-xact *value-expr-last-xact*)
	 (iterate ((xact xact-series))
	   (if last-xact
	       (let* ((basis
		       (subtract (xact-display-total xact)
				 (xact-display-amount xact)))
		      (difference (subtract basis
					    (xact-display-total last-xact))))
		 (unless (value-zerop difference)
		   (let ((new-entry
			  (make-instance 'entry
					 :journal journal
					 :actual-date (xact-date xact)
					 :payee ""))
			 (basis-total (xact-total last-xact)))
		     (add-to-contents journal new-entry)
		     (dolist (amount (if (balance-p difference)
					 (balance-amounts difference)
					 (list difference)))
		       (let ((new-xact
			      (make-transaction
			       :entry new-entry
			       :account  revaluation-account
			       :amount amount)))
			 (add-transaction new-entry new-xact)
			 (setf (xact-value new-xact :running-total)
			       (add basis-total amount))
			 (if displayed-amount-setter
			     (funcall displayed-amount-setter new-xact))
			 (next-out xacts-out new-xact)))))))
	   (next-out xacts-out xact)
	   (setf last-xact xact *value-expr-last-xact* last-xact))

	 (when (and last-xact todays-total)
	   ;; jww (2007-12-12): This requires a value expression that does not
	   ;; reference the xact passed
	   (let* ((todays-total (funcall todays-total last-xact))
		  (difference (subtract todays-total
					(xact-value last-xact :running-total))))
	     (unless (value-zerop difference)
	       (let* ((new-entry
		       (make-instance 'entry
				      :actual-date (now)
				      :payee ""))
		      (new-xact
		       (make-transaction :entry new-entry
					 :account revaluation-account
					 :amount difference)))
		 (add-transaction new-entry new-xact)
		 (setf (xact-value new-xact :running-total) todays-total)
		 (next-out xacts-out new-xact))))))))))

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
