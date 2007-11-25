;; This is a Ledger transform whose job is to normalize a binder.  This means
;; ensuring that it follows double-entry accounting rules, such that *all*
;; entries balance in terms of their cost basis.

;; jww (2007-11-06): This is not yet ported; at all.

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defvar *pre-normalization-functions* nil)
(defvar *post-normalization-functions* nil)

(defun normalize-entry (entry)
  (if (entry-normalizedp entry)
      (return-from normalize-entry entry))

  (if *pre-normalization-functions*
      (dolist (function *pre-normalization-functions*)
	(funcall function entry)))

  ;; Scan through and compute the total balance for the entry.  This
  ;; is used for auto-calculating the value of entries with no cost,
  ;; and the per-unit price of unpriced commodities.
  (let ((balance (make-instance 'balance))
        (no-amounts t)
        saw-null)
    (do-transactions (x entry)
      (let ((amt (get-xact-amount x)))
	(if (value-expr-p amt)
	    (setf (get-xact-amount x)
		  (value-expr-call amt x))))

      (when (or (not (xact-virtualp x))
		(xact-must-balance-p x))
	(let ((p (or (xact-cost x)
		     (get-xact-amount x))))
	  (if p
	      (progn
		(add* balance p)
		;;(format t " xact amount = ~S~%" (xact-amount x))
		;;(format t " xact cost = ~S~%" (xact-cost x))
		;;(format t " balance after add = ~S~%" (format-value balance))
		(if no-amounts
		    (setf no-amounts nil))

		(assert (get-xact-amount x))

		(if (and (xact-cost x)
			 (commodity-annotated-p
			  (amount-commodity (get-xact-amount x))))
		    (let* ((commodity (amount-commodity (get-xact-amount x)))
			   (price (annotation-price
				   (commodity-annotation commodity))))
		      (if price
			  (progn
			    ;;(format t " balance before = ~S~%" (format-value balance))
			    ;;(format t " price = ~S~%" price)
			    ;;(format t " (get-xact-amount x) = ~S~%" (get-xact-amount x))
			    ;;(format t " (multiply price (get-xact-amount x)) = ~S~%" (multiply price (get-xact-amount x)))
			    ;;(format t " (xact-cost x) = ~S~%" (xact-cost x))
			    ;;(format t " (subtract (multiply price (get-xact-amount x)) (xact-cost x)) = ~S~%" (subtract (multiply price (get-xact-amount x)) (xact-cost x)))
			    (add* balance
				  (subtract (multiply price (get-xact-amount x))
					    (xact-cost x)))
			    ;;(format t " balance after = ~S~%" (format-value balance))
			    )))))
	      (setf saw-null t)))))

    ;; If it's a null entry, then let the user have their fun
    (unless no-amounts
      ;; If there is only one transaction, balance against the default
      ;; account if one has been set.
      (let ((default-account (journal-default-account (entry-journal entry))))
	(when (and default-account
		   (= 1 (length (entry-transactions entry))))
	  (assert (not (value-zerop* balance)))
	  (let ((new-xact (make-transaction :entry entry
					    :status 'uncleared
					    :account default-account
					    :generatedp t)))
	    (add-transaction entry new-xact))
	  (setf saw-null t)))

      ;; If the first transaction of an entry with exactly two commodities is
      ;; of a different commodity than the following transactions, and it has
      ;; no per-unit price, determine its price by dividing the unit count
      ;; into the value of the balance.  This is done for the last eligible
      ;; commodity.
      (when (and (not saw-null)
		 (not (value-zerop* balance))
		 (= 2 (balance-commodity-count balance)))
	(let* ((x (first (entry-transactions entry)))
	       (commodity (amount-commodity (get-xact-amount x)))
	       (amount (copy-amount
			(amount-in-balance balance commodity)))
	       (balancing-amount
		(let ((amounts-map (get-amounts-map balance)))
		  (if (eq commodity (car (nth 0 amounts-map)))
		      (cdr (nth 1 amounts-map))
		      (cdr (nth 0 amounts-map)))))
	       (per-unit-cost (divide balancing-amount amount)))
	  ;;(format t "Auto-balancing at position ~S~%" (item-position-begin-char (entry-position entry)))
	  ;;(format t " amount	       = ~S~%" amount)
	  ;;(format t " cost	       = ~S~%" (xact-cost x))
	  ;;(format t " commodity        = ~S~%" commodity)
	  ;;(format t " balancing-amount = ~S~%" balancing-amount)
	  ;;(format t " per-unit-cost    = ~S~%" per-unit-cost)
	  (setf (amount-keep-precision-p amount) t)
	  ;;(format t " balance          = ~S~%" (format-value balance))
	  (loop
	     for x in (entry-transactions entry)
	     while x
	     unless (or (xact-cost x)
			(xact-virtualp x)
			(not (eq (amount-commodity amount)
				 commodity)))
	     do
	     ;;(format t " subtract ~S from balance~%" amount)
	     (subtract* balance amount)
	     ;;(format t " balance          = ~S~%" (format-value balance))
	     (if (and commodity (not (commodity-annotated-p commodity)))
		 (progn
		   ;;(format t "   amount before annotation = ~S~%" amount)
		   ;;(format t " 2.balance        = ~S~%" (format-value balance))
		   (setf (amount-commodity amount)
			 (annotate-commodity commodity
					     (make-commodity-annotation
					      :price (value-abs per-unit-cost)
					      :date  (entry-date entry)
					      :tag   (entry-code entry))))
		   ;;(format t " 3.balance        = ~S~%" (format-value balance))
		   ;;(format t "   amount before annotation = ~S~%" amount) 
		   ;;(format t " annotated commodity: ~S~%" (amount-commodity amount))
		   ))
	     ;;(format t "   per-unit-cost = ~S~%" per-unit-cost)
	     ;;(format t "   amount = ~S~%" amount)
	     ;;(format t "   (multiply per-unit-cost amount) = ~S~%" (multiply per-unit-cost amount))
	     ;;(format t "   (negate (multiply per-unit-cost amount)) = ~S~%" (negate (multiply per-unit-cost amount)))
	     ;;(format t " 4.balance        = ~S~%" (format-value balance))
	     (setf (xact-cost x) (negate (multiply per-unit-cost amount)))
	     ;;(format t " 5.balance        = ~S~%" (format-value balance))
	     ;;(format t " adding ~S to balance~%" (xact-cost x))
	     (add* balance (xact-cost x))
	     ;;(format t " 6.balance        = ~S~%" (format-value balance))
	     )))

      ;; Walk through each of the transactions, fixing up any that we
      ;; can, and performing any on-the-fly calculations.
      (let ((empty-allowed t))
	(do-transactions (x entry)
	  (unless (or (get-xact-amount x)
		      (and (xact-virtualp x)
			   (not (xact-must-balance-p x))))
	    (unless empty-allowed
	      (error "Only one transaction with null amount allowed per entry"))
	    (setf empty-allowed nil)

	    ;; If one transaction gives no value at all, its value will become
	    ;; the inverse of the value of the others.  If multiple
	    ;; commodities are involved, multiple transactions will be
	    ;; generated to balance them all.

	    (setf balance (optimize-value balance))

	    (if (balancep balance)
		(let ((first t))
		  (loop
		     for pairs = (get-amounts-map balance) then (cdr pairs)
		     while pairs
		     for pair = (car pairs)
		     do
		     (let ((amt (negate (cdr pair))))
		       (if first
			   (setf (get-xact-amount x) amt first nil)
			   (let ((new-xact
				  (make-transaction :entry entry
						    :account (xact-account x)
						    :amount amt
						    :generatedp t)))
			     (add-transaction entry new-xact)))
		       (add* balance amt))))
		(progn
		  (setf (get-xact-amount x) (negate balance)
			(xact-calculatedp x) t)
		  (add* balance (get-xact-amount x)))))))

      (if *post-normalization-functions*
	  (dolist (function *post-normalization-functions*)
	    (funcall function entry t))))

    (if (value-zerop balance)
	(prog1
	    entry
	  (setf (entry-normalizedp entry) t))
	(error "Entry does not balance (beg ~S end ~S); remaining balance is:~%~A"
	       (item-position-begin-char (entry-position entry))
	       (item-position-end-char (entry-position entry))
	       (format-value balance :width 20)))))

(provide 'normalize)
