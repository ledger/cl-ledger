;; entry.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun print-new-entry (xact-series &key (date nil) (payee nil) (account nil)
			(amount nil) (balance-account nil) (append nil)
			&allow-other-keys)
  (with-temporary-journal (journal)
    (let* ((entry (make-instance
		   'entry :journal journal
		   :actual-date (if date
				    (etypecase date
				      (string (strptime date))
				      (fixed-time date))
				    (fixed-time :hour 0))
		   :normalizedp nil))
	   (p-matcher (if payee
			  (payee-matcher payee)
			  (error "To derive an entry requires a payee")))
	   (a-matcher (and account (account-matcher account)))
	   (ba-matcher (and balance-account
			    (account-matcher balance-account)))
	   matching-xact
	   matching-account
	   matching-balance-account
	   referent-journal)
      (add-to-contents journal entry)

      (dolist (xact (nreverse (collect xact-series)))
	(unless referent-journal
	  (setf referent-journal (entry-journal (xact-entry xact))))

	(if (funcall p-matcher xact)
	    (setf matching-xact xact))
	(if (and a-matcher (funcall a-matcher xact))
	    (setf matching-account (xact-account xact)))
	(if (and ba-matcher (funcall ba-matcher xact))
	    (setf matching-balance-account (xact-account xact)))

	(if (and matching-xact
		 matching-account
		 matching-balance-account)
	    (return)))

      (unless matching-account
	(setf matching-account
	      (if (and matching-xact (null account))
		  (xact-account (first (entry-transactions
					(xact-entry matching-xact))))
		  (find-account journal (or account "Expenses:Unknown")
				:create-if-not-exists-p t))))

      (typecase amount
	(string (setf amount (cambl:amount amount)))
	(integer (setf amount (integer-to-amount amount))))

      (if (null matching-xact)
	  (progn
	    (setf (entry-payee entry) payee)
	    (add-transaction
	     entry
	     (make-transaction :entry entry
			       :account matching-account
			       :amount amount)))
	  (let ((new-xact
		 (make-transaction :entry   entry
				   :account matching-account)))
	    (add-transaction entry new-xact)
	    (setf (entry-payee entry)
		  (entry-payee (xact-entry matching-xact)))
	    (if matching-account
		(if amount
		    (progn
		      (unless (amount-commodity amount)
			(setf (amount-commodity amount)
			      (amount-commodity (xact-amount matching-xact))
			      (amount-keep-precision-p amount)
			      (amount-keep-precision-p
			       (xact-amount matching-xact))))
		      (setf (xact-amount new-xact) amount))
		    (setf (xact-amount new-xact)
			  (xact-amount matching-xact)))
		(setf entry (xact-entry matching-xact)))))

      (unless matching-balance-account
	(if balance-account
	    (setf matching-balance-account
		  (find-account referent-journal
				balance-account
				:create-if-not-exists-p t))
	    (when matching-xact
	      (dolist (xact (reverse (entry-transactions
				      (xact-entry matching-xact))))
		(when (xact-calculatedp xact)
		  (setf matching-balance-account
			(xact-account xact))
		  (return))))))

      (add-transaction
       entry
       (make-transaction
	:entry entry
	:account (or matching-balance-account
		     (journal-default-account referent-journal)
		     (find-account referent-journal
				   "Assets:Unknown"
				   :create-if-not-exists-p t))))

      (if append
	  (with-open-file (out (journal-source referent-journal)
			       :direction :output :if-exists :append)
	    (format out "~&~%")
	    (print-entry entry :output-stream out))
	  (print-entry entry))
      entry))) 

(defun derive-entry (&rest args)
  "The DERIVE-ENTRY report uses Ledger to intelligently create a new entry for
you.  The possible keywords arguments are:

    :DATE             <DATE-STRING>
    :PAYEE            <REGEXP>
    :ACCOUNT          <REGEXP>
    :BALANCE-ACCOUNT  <REGEXP>
    :AMOUNT           <VALUE-STRING>
    :APPEND           <BOOLEAN>

Except for :PAYEE, all of these keyword arguments are optional.  Here is what
they mean:

  :PAYEE REGEXP
    Find the most recent entry whose payee matches REGEXP, and base the new
    entry derivation on its details.  If no matching entry can be found, the
    payee of the newly created entry will exactly match REGEXP.

  :DATE DATE-STRING
    The date of the new entry will be DATE-STRING, otherwise it is today.

  :ACCOUNT REGEXP
    Set the first account line in the new entry to be the most recently used
    account which matches REGEXP.  If no such account can be found, an account
    named REGEXP is used.  If no account is specified, the account
    \"Expenses:Unknown\" is used.

  :BALANCE-ACCOUNT REGEXP
    Like :ACCOUNT, except this refers to the account used for the second
    transaction in the newly derived entry.  If not specified, a calculated
    \"balance account\" is looked for in the matching entry; if this does not
    apply, the journal's default account is used; if this does not apply, the
    account \"Asets:Unknown\" is used.

  :AMOUNT VALUE-STRING
    The amount of the first transaction.  If it has no commodity, the
    correlated commodity from the discovered entry is used.

  :APPEND BOOLEAN
    If non-NIL, the new entry is written to the same journal where the
    matching entry was found (for a binder that references many journals, this
    is whichever file the discovered entry was in).

  Here are a few examples, using sample.dat as a reference:

  (ledger:derive-entry \"doc/sample.dat\" :payee \"book\")
    =>
    2007/12/04 Book Store
        Expenses:Books                            $20.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee \"book\" :amount \"$125\")
    =>
    2007/12/04 Book Store
        Expenses:Books                           $125.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee \"Hello World\")
    =>
    2007/12/04 Hello World
        Expenses:Unknown
        Assets:Unknown

  (ledger:derive-entry :date \"2004/01/01\" :payee \"Hello World\")
    =>
    2004/01/01 Hello World
        Expenses:Unknown
        Assets:Unknown

  (ledger:derive-entry :payee \"book\" :account \"equ\")
    =>
    2007/12/04 Book Store
        Equity:Opening Balances                   $20.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee \"book\" :account \"Who Knows\")
    =>
    2007/12/04 Book Store
        Who Knows                                 $20.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee \"book\" :balance-account \"bank\")
    =>
    2007/12/04 Book Store
        Expenses:Books                            $20.00
        Assets:Bank:Checking

  (ledger:derive-entry :payee \"book\" :account \"liab\" 
                       :balance-account \"bank\")
    =>
    2007/12/04 Book Store
        Liabilities:MasterCard                   $-20.00
        Assets:Bank:Checking

  (ledger:derive-entry :payee \"book\" :account \"bank\" :amount 50)
    =>
    2007/12/04 Book Store
        Assets:Bank:Checking                      $50.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee \"book\" :account \"bank\" :amount \"$125\")
    =>
    2007/12/04 Book Store
        Assets:Bank:Checking                     $125.00
        Liabilities:MasterCard"
  (basic-reporter #'print-new-entry
		  (append args (list :accounts-report t
				     :inhibit-filter t))))

(provide 'entry)

;; entry.lisp ends here
