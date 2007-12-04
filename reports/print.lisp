;; print.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun print-entry (entry &key (output-stream *standard-output*))
  (format output-stream "~&~A ~A~%" (strftime (entry-date entry))
	  (entry-payee entry))

  (dolist (xact (entry-transactions entry))
    (let ((amount (if (entry-normalizedp entry)
		      (xact-amount xact)
		      (xact-amount-expr xact)))
	  (cost (xact-cost xact)))
      (format output-stream "    ~35A ~12A"
	      (account-fullname (xact-account xact))
	      (if (or (null amount) (xact-calculatedp xact)) ""
		  (if (stringp amount) amount
		      (format-value amount :width 12 :latter-width 52))))
      (if cost
	  (format output-stream " @ ~A"
		  (format-value (divide cost amount))))
      (format output-stream "~%"))))

(defun print-reporter (&key (output-stream *standard-output*))
  (let (last-entry)
    (lambda (xact)
      ;; First display the entry details, if it would not be repeated
      (when (or (null last-entry)
		(not (eq last-entry (xact-entry xact))))
	(if last-entry
	    (format output-stream "~%")
	    (format output-stream "~&"))

	(format output-stream "~A ~A~%"
		(strftime (entry-date (xact-entry xact)))
		(entry-payee (xact-entry xact)))
	(setf last-entry (xact-entry xact)))

      ;; Then display the transaction details; if this is an unnormalized,
      ;; then display exactly what was specified 
      (let ((amount (if (entry-normalizedp last-entry)
			(xact-amount xact)
			(xact-amount-expr xact)))
	    (cost (xact-cost xact)))
	(format output-stream "    ~35A ~12A"
		(account-fullname (xact-account xact))
		(if (or (null amount) (xact-calculatedp xact)) ""
		    (if (stringp amount) amount
			(format-value amount :width 12 :latter-width 52))))
	(if cost
	    (format output-stream " @ ~A"
		    (format-value (divide cost amount)))))

      (format output-stream "~%"))))

(defun print-transactions (xact-series &key (reporter nil) (no-total nil)
			   &allow-other-keys)
  (declare (ignore no-total))
  (let ((reporter (or reporter (print-reporter))))
    (iterate ((xact xact-series))
      (funcall reporter xact))))

(defun print-report (&rest args)
  (basic-reporter #'print-transactions args))

(defun equity-report (&rest args)
  (with-temporary-journal (journal)
    (let ((equity-account (find-account journal
					"Equity:Opening Balances"
					:create-if-not-exists-p t)))
      (multiple-value-bind (xact-series plist)
	  (find-all-transactions (append args (list :subtotal t)))
	(dolist (entry-xacts (group-transactions-by-entry
			      (collect xact-series)))
	  (let ((entry (copy-entry (xact-entry (car entry-xacts))
				   :journal journal
				   :normalizedp nil)))
	    (add-to-contents journal entry)
	    (dolist (xact entry-xacts)
	      (let ((xact-copy (copy-transaction xact)))
		(setf (xact-entry xact-copy) entry)
		(add-transaction entry xact-copy)))
	    (add-transaction entry
			     (make-transaction :entry entry
					       :account equity-account))
	    entry))
	(apply #'print-transactions (scan-transactions journal) plist)))))

(provide 'print)

;; print.lisp ends here
