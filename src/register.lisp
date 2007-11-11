;; register.lisp

(declaim (optimize (safety 3) (debug 3)))

(in-package :ledger)

;; (put 'do-transactions 'lisp-indent-function 1)

(defun register-report (binder &key (output-stream *standard-output*))
  (declare (type binder binder))
  (declare (type stream output-stream))
  (do-transactions (xact binder)
    (format output-stream "~10A ~20A ~22A ~A ~A~%"
	    "DATE"
	    (entry-payee (xact-entry xact))
	    (account-fullname (xact-account xact))
	    (format-value (xact-amount xact)
			  :width 12 :latter-width 67)
	    (let ((running-total (xact-value :running-total xact)))
	      (if running-total
		  (format-value running-total
				:width 12 :latter-width 80)
		  ""))))
  (values))

(export 'register-report)

(provide 'register)

;; register.lisp ends here
