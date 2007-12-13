;; balance.lisp

(in-package :ledger)

(defun balance-print-reporter (&optional (no-total nil)
			       (output-stream *standard-output*))
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
		    (and subtotal (not (value-zerop subtotal)))
		    (> (account-value account :children-with-totals) 1))
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

(defun report-accounts (account reporter &optional (elided 0) (real-depth 0))
  (if (plusp real-depth)
      (unless (funcall reporter account real-depth elided)
	(incf elided)))

  (if (account-children account)
      (locally
	  #+sbcl (declare (sb-ext:muffle-conditions
			   sb-ext:code-deletion-note))
	  (mapc #'(lambda (cell)
		    (report-accounts (cdr cell) reporter elided
				     (1+ real-depth)))
		(sort (let (lst)
			(maphash #'(lambda (key value)
				     (push (cons key value) lst))
				 (account-children account))
			lst)
		      #'string< :key #'car)))))

(defun balance-reporter (actual-reporter)
  (lambda (account)
    (report-accounts account actual-reporter)
    (funcall actual-reporter account 0 0 t)
    nil))

(defun balance-report (&rest args)
  (let ((output-stream (member :output-stream args))
	(no-total (member :no-total args)))
    (accounts-report
     (append args (list :reporter
			(balance-reporter
			 (balance-print-reporter
			  (and no-total
			       (cadr no-total))
			  (if output-stream
			      (cadr output-stream)
			      *standard-output*))))))))

(provide 'balance)

;; balance.lisp ends here
