;; collapse.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun collapse-entries (xact-series)
  (with-temporary-journal (journal)
    (let ((total-account (find-account journal "<Total>"
				       :create-if-not-exists-p t)))
      (map-fn
       'transaction
       #'(lambda (entry-xacts)
	   (let ((entry (copy-entry (xact-entry (car entry-xacts))
				    :journal journal
				    :normalizedp t)))
	     (add-to-contents journal entry)
	     (add-transaction
	      entry
	      (if (= 1 (length entry-xacts))
		  (first entry-xacts)
		  (reduce #'(lambda (total-xact xact)
			      (add* (get-xact-amount total-xact)
				    (xact-amount xact))
			      total-xact)
			  entry-xacts
			  :initial-value
			  (make-transaction :entry entry
					    :account total-account
					    :amount (balance)))))))
       (scan (group-transactions-by-entry (collect xact-series)))))))

(provide 'collapse)

;; collapse.lisp ends here
