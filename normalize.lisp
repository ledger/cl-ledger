;; This is a Ledger transform whose job is to normalize a binder.  This means
;; ensuring that it follows double-entry accounting rules, such that *all*
;; entries balance in terms of their cost basis.

;; jww (2007-11-06): This is not yet ported; at all.

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun normalize-entry (entry)
  ;; Scan through and compute the total balance for the entry.  This
  ;; is used for auto-calculating the value of entries with no cost,
  ;; and the per-unit price of unpriced commodities.
  (let ((balance (make-instance 'balance))
        (no-amounts t)
        saw-null)
    (do-transactions (x entry)
      (let ((amt (xact-amount x)))
	(if (functionp amt)
	    (setf (xact-amount x) (funcall amt x))))

      (when (or (not (xact-virtualp x))
		(xact-must-balance-p x))
	(let ((p (or (xact-cost x)
		     (xact-amount x))))
	  (if p
	      (progn
		(add* balance p)
		(if no-amounts
		    (setf no-amounts nil))

		(assert (xact-amount x))

		(if (and (xact-cost x)
			 (commodity-annotated-p
			  (amount-commodity (xact-cost x))))
		    (let* ((commodity (amount-commodity (xact-cost x)))
			   (price (annotation-price
				   (commodity-annotation commodity))))
		      (if price
			  (add* balance
				(subtract (multiply price (xact-amount x))
					  (xact-cost x)))))))
	      (setf saw-null t)))))

    ;; If it's a null entry, then let the user have their fun
    (unless no-amounts
      ;; If there is only one transaction, balance against the default
      ;; account if one has been set.
      (when (and *default-account*
		 (= 1 (length (entry-transactions entry))))
	(assert (not (value-zerop* balance)))
	(add-transaction entry (make-transaction
				:entry entry
				:status 'uncleared
				:account *default-account*
				:generatedp t))
	(setf saw-null t))

      ;; If the first transaction of an entry with exactly two commodities is
      ;; of a different commodity than the following transactions, and it has
      ;; no per-unit price, determine its price by dividing the unit count
      ;; into the value of the balance.  This is done for the last eligible
      ;; commodity.
      (when (and (not saw-null)
		 (not (value-zerop* balance))
		 (= 2 (balance-commodity-count balance)))
	(let* ((xact-iterator (transactions-iterator entry))
	       (x (funcall xact-iterator))
	       (amount (xact-amount x))
	       (commodity (amount-commodity amount))
	       (balancing-amount
		(let ((amounts-map (cambl::get-amounts-map balance)))
		  (if (eq commodity (car (nth 0 amounts-map)))
		      (cdr (nth 1 amounts-map))
		      (cdr (nth 0 amounts-map)))))
	       (per-unit-cost
		(divide balancing-amount amount)))
	  (setf (amount-keep-precision-p amount) t)
	  (loop
	     for x = (funcall xact-iterator)
	     while x
	     unless (or (null (xact-cost x))
			(xact-virtualp x)
			(not (eq (amount-commodity amount)
				 commodity)))
	     do
	     (subtract* balance amount)
	     (if (and commodity
		      (not (commodity-annotated-p commodity)))
		 (annotate-commodity commodity
				     (make-commodity-annotation
				      :price (value-abs per-unit-cost)
				      :date  (entry-date entry)
				      :tag   (entry-code entry))))
	     (setf (xact-cost x)
		   (negate (multiply per-unit-cost amount)))
	     (add* balance (xact-cost x)))))

      ;; Walk through each of the transactions, fixing up any that we
      ;; can, and performing any on-the-fly calculations.
      (let ((empty-allowed t))
	(do-transactions (x entry)
	  (unless (or (xact-amount x)
		      (and (xact-virtualp x)
			   (not (xact-must-balance-p x))))
	    (unless empty-allowed
	      (error "Only one transaction with null amount allowed per entry"))
	    (setf empty-allowed nil))

	  ;; If one transaction gives no value at all, its value will become
	  ;; the inverse of the value of the others.  If multiple commodities
	  ;; are involved, multiple transactions will be generated to balance
	  ;; them all.

	  (setf balance (optimize-value balance))

	  (if (balancep balance)
	      (let ((first t))
		(loop
		   for pairs = (cambl::get-amounts-map balance)
		   then (cdr pairs)
		   for pair = (car pairs)
		   do
		   (let ((amt (negate (cdr pair))))
		     (if first
			 (progn
			   (setf (xact-amount x) amt
				 first nil))
			 (let ((new-xact
				(make-transaction :entry entry
						  :account (xact-account x)
						  :amount amt
						  :generatedp t)))
			   (add-transaction entry new-xact)))
		     (add* balance amt)))
		)
	      (progn
		(setf (xact-amount x) (negate balance)
		      (xact-calculatedp x) t)
		(add* balance (xact-amount x))))))

      (if (value-zerop* balance)
	  entry
	  (error "Entry does not balance")))))

(defun normalize-binder (binder)
  (let ((entry-class (find-class 'entry)))
    (dolist (journal (binder-journals binder))
      (setf (journal-entries journal)
            (loop
               for entry in (journal-entries journal)
               when (eq entry-class (class-of entry))
               collect (normalize-entry entry))))
    (assert (null (binder-transactions binder))))
  binder)

(export 'normalize-binder)

(provide 'normalize)
