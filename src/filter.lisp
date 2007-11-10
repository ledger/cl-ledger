;; filter.lisp

(declaim (optimize (safety 3) (debug 3)))

(in-package :ledger)
(use-package :cl-ppcre)

(defun destructively-filter (binder &key (account nil) (payee nil))
  (let ((account-scanner (and account (cl-ppcre:create-scanner account)))
	(payee-scanner (and payee (cl-ppcre:create-scanner payee))))
    (dolist (journal (binder-journals binder))
      (setf (journal-entries journal)
	    (loop for entry in (journal-entries journal)
	       when (or (null payee-scanner)
			(cl-ppcre:scan payee-scanner (entry-payee entry)))
	       do (setf (entry-transactions entry)
			(loop for xact in (entry-transactions entry)
			   when (or (null account-scanner)
				    (cl-ppcre:scan account-scanner
						   (account-fullname
						    (xact-account xact))))
			   collect xact))
	       collect entry))))
  binder)

(export 'destructively-filter)

;; filter.lisp ends here
