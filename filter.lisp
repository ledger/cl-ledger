;; filter.lisp

(declaim (optimize (safety 3) (debug 3)))

(in-package :ledger)
(use-package :cl-ppcre)

(defun include-transaction-p (xact &key
			     (account-scanner nil)
			     (payee-scanner nil)
			     (note-scanner nil)
			     (expr-closure nil))
  (let ((entry (xact-entry xact)))
    (and (or (null account-scanner)
	     (cl-ppcre:scan account-scanner
			    (account-fullname (xact-account xact))))
	 (or (null payee-scanner)
	     (cl-ppcre:scan payee-scanner (entry-payee entry)))
	 (or (null note-scanner)
	     (null (xact-note xact))
	     (cl-ppcre:scan note-scanner (xact-note xact))
	     (and (entry-note entry)
		  (cl-ppcre:scan note-scanner (entry-note entry))))
	 (or (null expr-closure)
	     (value-truth (funcall expr-closure xact))))))

(defun destructively-filter (binder &key
			     (account nil)
			     (payee nil)
			     (note nil)
			     (expr nil))
  (let ((account-scanner
	 (and account (cl-ppcre:create-scanner account :case-insensitive-mode t)))
	(payee-scanner
	 (and payee (cl-ppcre:create-scanner payee :case-insensitive-mode t)))
	(note-scanner
	 (and note (cl-ppcre:create-scanner note :case-insensitive-mode t)))
	(expr-closure (and expr (parse-value-expr expr))))
    (when (or account-scanner
	      payee-scanner
	      note-scanner
	      expr-closure)
      (dolist (journal (binder-journals binder))
	(setf (journal-entries journal)
	      (loop
		 for entry in (journal-entries journal)
		 for match-p =
		 (and (or (null payee-scanner)
			  (cl-ppcre:scan payee-scanner
					 (entry-payee entry)))
		      (or (null note-scanner)
			  (null (entry-note entry))
			  (cl-ppcre:scan note-scanner
					 (entry-note entry))))
		 do
		 (if (and match-p (or account-scanner
				      note-scanner
				      expr-closure))
		     (setf (entry-transactions entry)
			   (loop
			      for xact in (entry-transactions entry)
			      when (include-transaction-p
				    xact
				    :account-scanner account-scanner
				    :note-scanner    note-scanner
				    :expr-closure    expr-closure)
			      collect xact)))
		 when match-p collect entry)))

      (if account-scanner
	  (labels
	      ((filter-in-account (name account)
		 (declare (ignore name))
		 (setf (account-transactions account)
		       (loop
			  for xact in (account-transactions account)
			  when (include-transaction-p
				xact
				:account-scanner account-scanner
				:payee-scanner   payee-scanner
				:note-scanner    note-scanner
				:expr-closure    expr-closure)
			  collect xact))
		 (let ((children (account-children account)))
		   (if children
		       (maphash #'filter-in-account children)))))
	    (filter-in-account "" (binder-root-account binder))))

      (if (binder-transactions binder)
	  (setf (binder-transactions binder)
		(loop
		   for xact in (binder-transactions binder)
		   when (include-transaction-p
			 xact
			 :account-scanner account-scanner
			 :payee-scanner   payee-scanner
			 :note-scanner    note-scanner
			 :expr-closure    expr-closure)
		   collect xact)))))
  binder)

(export 'destructively-filter)

(provide 'filter)

;; filter.lisp ends here
