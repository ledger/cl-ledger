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
      (if (or (not (xact-virtualp x))
	      (xact-must-balance-p x))
	  t
	  ;; amount_t& p((*x)->cost ? *(*x)->cost : (*x)->amount);
	  ;; if (p) {
	  ;;     if (no_amounts) {
	  ;;       balance = p;
	  ;;       no_amounts = false;
	  ;;     } else {
	  ;;       balance += p;
	  ;;     }
	  ;; 
	  ;;     assert((*x)->amount);
	  ;;     if ((*x)->cost && (*x)->amount.commodity().annotated) {
	  ;;       annotated_commodity_t&
	  ;;         ann_comm(static_cast<annotated_commodity_t&>
	  ;;                  ((*x)->amount.commodity()));
	  ;;       if (ann_comm.details.price)
	  ;;         balance += (*ann_comm.details.price * (*x)->amount.number() -
	  ;;                     *((*x)->cost));
	  ;;     }
	  ;; } else {
	  ;;     saw_null = true;
	  ;; }
	  ))

    ;; If it's a null entry, then let the user have their fun
    (unless no-amounts

      ;; If there is only one transaction, balance against the basket
      ;; account if one has been set.

      (when (and *default-account*
		 (= 1 (length (entry-transactions entry))))
	;; assert(balance.is_amount());
	;; transaction_t * nxact = new transaction_t(journal->basket);
	;; // The amount doesn't need to be set because the code below will
	;; // balance this transaction against the other.
	;; add_transaction(nxact);
	;; nxact->add_flags(TRANSACTION_CALCULATED);
	)

      ;; If the first transaction of a two-transaction entry is of a different
      ;; commodity than the other, and it has no per-unit price, determine its
      ;; price by dividing the unit count into the value of the balance.  This
      ;; is done for the last eligible commodity.

      (when (and (not saw-null)
		 (not (value-zerop* balance))
		 (= 2 (balance-commodity-count balance)))
	;; transactions_list::const_iterator x = transactions.begin();
	;; assert((*x)->amount);
	;; commodity_t& this_comm = (*x)->amount.commodity();
	;; 
	;; balance_t::amounts_map::const_iterator this_bal =
	;;   balance.as_balance().amounts.find(&this_comm);
	;; balance_t::amounts_map::const_iterator other_bal =
	;;   balance.as_balance().amounts.begin();
	;; if (this_bal == other_bal)
	;;   other_bal++;
	;; 
	;; amount_t per_unit_cost =
	;;   amount_t((*other_bal).second / (*this_bal).second.number()).unround();
	;; 
	;; for (; x != transactions.end(); x++) {
	;;   if ((*x)->cost || (*x)->has_flags(TRANSACTION_VIRTUAL) ||
	;;         (*x)->amount.commodity() != this_comm)
	;;       continue;
	;; 
	;;   balance -= (*x)->amount;
	;; 
	;;   entry_t * entry = dynamic_cast<entry_t *>(this);
	;; 
	;;   if ((*x)->amount.commodity() &&
	;;         ! (*x)->amount.commodity().annotated)
	;;       (*x)->amount.annotate_commodity
	;;         (annotation_t(per_unit_cost.abs(),
	;;                       entry ? entry->actual_date() : optional<moment_t>(),
	;;                       entry ? entry->code          : optional<string>()));
	;; 
	;;   (*x)->cost = - (per_unit_cost * (*x)->amount.number());
	;;   balance += *(*x)->cost;
	;; }
	)

      ;; Walk through each of the transactions, fixing up any that we
      ;; can, and performing any on-the-fly calculations.

      (let ((empty-allowed t))
	(do-transactions (x entry)
	  ;; if ((*x)->amount ||
	  ;;       ((*x)->has_flags(TRANSACTION_VIRTUAL) &&
	  ;;        ! (*x)->has_flags(TRANSACTION_BALANCE)))
	  ;;   continue;
	  ;; 
	  ;; if (! empty_allowed)
	  ;;   throw_(std::logic_error,
	  ;;            "Only one transaction with null amount allowed per entry");
	  ;; empty_allowed = false;
	  ;; 
	  ;; // If one transaction gives no value at all, its value will become
	  ;; // the inverse of the value of the others.  If multiple
	  ;; // commodities are involved, multiple transactions will be
	  ;; // generated to balance them all.
	  ;; 
	  ;; const balance_t * bal = NULL;
	  ;; switch (balance.type()) {
	  ;; case value_t::BALANCE_PAIR:
	  ;;   bal = &balance.as_balance_pair().quantity();
	  ;;   // fall through...
	  ;; 
	  ;; case value_t::BALANCE:
	  ;;   if (! bal)
	  ;;       bal = &balance.as_balance();
	  ;; 
	  ;;   if (bal->amounts.size() < 2) {
	  ;;       balance.cast(value_t::AMOUNT);
	  ;;   } else {
	  ;;       bool first = true;
	  ;;       for (balance_t::amounts_map::const_iterator
	  ;;              i = bal->amounts.begin();
	  ;;            i != bal->amounts.end();
	  ;;            i++) {
	  ;;         amount_t amt = (*i).second.negate();
	  ;; 
	  ;;         if (first) {
	  ;;           (*x)->amount = amt;
	  ;;           first = false;
	  ;;         } else {
	  ;;           transaction_t * nxact = new transaction_t((*x)->account);
	  ;;           add_transaction(nxact);
	  ;;           nxact->add_flags(TRANSACTION_CALCULATED);
	  ;;           nxact->amount = amt;
	  ;;         }
	  ;; 
	  ;;         balance += amt;
	  ;;       }
	  ;;       break;
	  ;;   }
	  ;;   // fall through...
	  ;; 
	  ;; case value_t::AMOUNT:
	  ;;   (*x)->amount = balance.as_amount().negate();
	  ;;   (*x)->add_flags(TRANSACTION_CALCULATED);
	  ;; 
	  ;;   balance += (*x)->amount;
	  ;;   break;
	  ;; 
	  ;; default:
	  ;;   break;
	  ;; }
	  ))

      (unless (value-zerop* balance)
	(error "Entry does not balance")))

    t))

(defun normalize-binder (binder)
  (let ((zero-amount (integer-to-amount 0))
        (entry-class (find-class 'entry)))
    (dolist (journal (binder-journals binder))
      (setf (journal-entries journal)
            (loop
               for entry in (journal-entries journal)
               do (dolist (xact (entry-transactions entry))
                    (let ((amt (xact-amount xact)))
                      (cond
                        ((null amt)
                         (setf (xact-amount xact) zero-amount))
                        ((functionp amt)
                         (setf (xact-amount xact)
                               (funcall (xact-amount xact) xact))))))
               when (eq entry-class (class-of entry))
               collect entry)))
    (assert (null (binder-transactions binder))))
  binder)

(export 'normalize-binder)

(provide 'normalize)
