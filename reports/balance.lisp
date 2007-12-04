;; balance.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun balance-reporter (&key (output-stream *standard-output*)
			 (no-total nil))
  (labels
      ((get-partial-name (string account count)
	 (if (zerop count)
	     string
	     (progn
	       (setf account (account-parent account))
	       (get-partial-name (concatenate 'string (account-name account)
					      ":" string)
				 account (1- count))))))
    ;; This printer function returns t if it decided to display the account,
    ;; or nil otherwise.  If an account is not printed, its child will see a
    ;; value for "elided" representing how many generations elected not to
    ;; display.
    (let ((amounts-width 16))
     (lambda (account real-depth elided &optional finalp)
       (let ((subtotal (account-value account :subtotal)))
	 (when (or finalp
		   (or (and subtotal
			    (not (value-zerop (account-value account :subtotal))))
		       (> (account-value account :children-with-totals) 1)))
	   (if (not finalp)
	       (format output-stream "~A  ~vA~A~%"
		       (format-value (account-value account :total)
				     :width amounts-width)
		       (* (- (1- real-depth) elided) 2) ""
		       (get-partial-name (account-name account) account
					 elided))
	       (unless no-total
		 (format output-stream
			 "-----------------------------------------------------~%~A~%"
			 (format-value (account-value account :total)
				       :width amounts-width))))
	   t))))))

(defun print-balance (xact-series &key (reporter nil) (no-total nil)
		      &allow-other-keys)
  (let ((root-account (calculate-account-totals xact-series))
	(reporter (or reporter (balance-reporter :no-total no-total))))
    (labels
	((print-accounts (account elided real-depth)
	   (if (plusp real-depth)
	       (unless (funcall reporter account real-depth elided)
		 (incf elided)))

	   (if (account-children account)
	       (locally #+sbcl(declare (sb-ext:muffle-conditions
					sb-ext:code-deletion-note))
			(mapc #'(lambda (cell)
				  (print-accounts (cdr cell) elided
						  (1+ real-depth)))
			      (sort (let (lst)
				      (maphash #'(lambda (key value)
						   (push (cons key value) lst))
					       (account-children account))
				      lst)
				    #'string< :key #'car))))))
      (when root-account
	(print-accounts root-account 0 0)
	(funcall reporter root-account 0 0 t)))))

(defun balance-report (&rest args)
  (basic-reporter #'print-balance (append args (list :accounts-report t))))

(provide 'balance)

;; balance.lisp ends here
