;; register.lisp

(declaim (optimize (safety 3) (debug 3)))

(in-package :ledger)

(defun register (binder &key (output-stream *standard-output*))
  (dolist (journal (binder-journals binder))
    (dolist (entry (journal-entries journal))
      (dolist (xact (entry-transactions entry))
	(format output-stream "~10A ~20A ~22A ~A ~A~%"
		"DATE"
		(entry-payee entry)
		(account-fullname (xact-account xact))
		(format-value (xact-amount xact)
			      :width 12 :latter-width 67)
		(format-value (xact-value :running-total xact)
			      :width 12 :latter-width 80)))))
  binder)

(export 'register)

;; register.lisp ends here
