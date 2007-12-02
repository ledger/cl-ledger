;; print.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

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
		(strftime (xact-date xact))
		(entry-payee (xact-entry xact)))
	(setf last-entry (xact-entry xact)))

      ;; Then display the transaction details
      (let ((amount (xact-amount xact))
	    (cost (xact-cost xact)))
	(format output-stream "    ~35A ~12A"
		(account-fullname (xact-account xact))
		(if (xact-calculatedp xact) ""
		    (format-value amount :width 12 :latter-width 52)))
	(if cost
	    (format output-stream " @ ~A"
		    (format-value (divide cost amount)))))

      (format output-stream "~%"))))

(defun print-transactions (xact-series &key (reporter nil) (no-total nil))
  (declare (ignore no-total))
  (let ((reporter (or reporter (print-reporter))))
    (iterate ((xact xact-series))
      (funcall reporter xact))))

(defun print-report (&rest args)
  (basic-reporter #'print-transactions args))

(provide 'print)

;; print.lisp ends here
