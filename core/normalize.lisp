;; This function ensures that an entry follows double-entry accounting rules,
;; so that it must balance in terms of its cost basis.  It also computes the
;; cost where this has no been explicitly specified.

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

  ;; Scan through and compute the total balance for the entry.  This is used
  ;; for auto-calculating the value of entries with no cost, and the per-unit
  ;; price of unpriced commodities.
  (let ((balance 0)
        null-xact)
    (do-transactions (xact entry)
      (when (xact-must-balance-p xact)
	(let ((amt (xact-amount* xact)))
	  (if amt
	      (setf balance (add balance (or (xact-cost xact) amt)))
	      (if null-xact
		  (error "Only one transaction with null amount allowed ~
                          per entry (beg ~S end ~S)"
			 (item-position-begin-line (entry-position entry))
			 (item-position-end-line (entry-position entry)))
		  (setf null-xact xact))))))

    ;; If there is only one transaction, balance against the default account
    ;; if one has been set.
    (when (= 1 (length (entry-transactions entry)))
      (if-let ((default-account
		   (journal-default-account (entry-journal entry))))
	(setf null-xact
	      (make-transaction :entry entry
				:status (xact-status
					 (first (entry-transactions entry)))
				:account default-account
				:generatedp t))
	(add-transaction entry null-xact)))

    (if null-xact
	;; If one transaction has no value at all, its value will become the
	;; inverse of the rest.  If multiple commodities are involved,
	;; multiple transactions are generated to balance them all.
	(progn
	  (if (balance-p balance)
	      (let ((first t))
		(dolist (amount (balance-amounts balance))
		  (if first
		      (setf (xact-amount* null-xact) (negate amount)
			    first nil)
		      (add-transaction
		       entry
		       (make-transaction :entry entry
					 :account (xact-account null-xact)
					 :amount (negate amount)
					 :generatedp t)))))
	      (setf (xact-amount* null-xact) (negate balance)
		    (xact-calculatedp null-xact) t))

	  (setf balance 0))

	;; When an entry involves two different commodities (regardless of how
	;; many transactions there are) determine the conversion ratio by
	;; dividing the total value of one commodity by the total value of the
	;; other.  This establishes the per-unit cost for this transaction for
	;; both commodities.
	(when (and (balance-p balance)
		   (= 2 (balance-commodity-count balance)))
	  (destructuring-bind (x y) (balance-amounts balance)
	    (let ((a-commodity (amount-commodity x))
		  (per-unit-cost (value-abs (divide x y))))
	      (do-transactions (xact entry)
		(let ((amount (xact-amount* xact)))
		  (unless (or (xact-cost xact)
			      (not (xact-must-balance-p xact))
			      (commodity-equal (amount-commodity amount)
					       a-commodity))
		    (setf balance (subtract balance amount)
			  (xact-cost xact) (multiply per-unit-cost amount)
			  balance (add balance (xact-cost xact))))))))))

    ;; Now that the transaction list has its final form, calculate the balance
    ;; once more in terms of total cost, accounting for any possible gain/loss
    ;; amounts.
    (do-transactions (xact entry)
      (when (xact-cost xact)
	(let ((amount (xact-amount* xact)))
	  (assert (not (commodity-equal (amount-commodity amount)
					(amount-commodity (xact-cost xact)))))
	  (multiple-value-bind (annotated-amount total-cost basis-cost)
	      (exchange-commodity amount :total-cost (xact-cost xact)
				  :moment (entry-date entry)
				  :tag (entry-code entry))
	    (if (annotated-commodity-p (amount-commodity amount))
		(if-let ((price (annotation-price
				 (commodity-annotation
				  (amount-commodity amount)))))
		  (setf balance
			(add balance (subtract basis-cost total-cost))))
		(setf (xact-amount* xact) annotated-amount))))))

    (if *post-normalization-functions*
	(dolist (function *post-normalization-functions*)
	  (funcall function entry t)))

    (if (value-zerop balance)
	(prog1
	    entry
	  (setf (entry-normalizedp entry) t))
	(error "Entry does not balance (beg ~S end ~S); remaining balance is:~%~A"
	       (item-position-begin-line (entry-position entry))
	       (item-position-end-line (entry-position entry))
	       (format-value balance :width 20)))))

(provide 'normalize)
