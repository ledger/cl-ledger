;; This is a Ledger transform whose job is to normalize a binder.  This means
;; ensuring that it follows double-entry accounting rules, such that *all*
;; entries balance in terms of their cost basis.

;; jww (2007-11-06): This is not yet ported; at all.

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun normalize-binder (binder)
  (let ((zero-amount (integer-to-amount 0)))
    (dolist (journal (binder-journals binder))
      (setf (journal-entries journal)
	    (loop
	       for entry in (journal-entries journal)
	       do (dolist (xact (entry-transactions entry))
		    (let ((amt (xact-amount xact)))
		      (cond
			((null amt)
			 (setf (xact-amount xact) zero-amount))
			((functionp amt)
			 (setf (xact-amount xact)
			       (funcall (xact-amount xact) xact))))))
	       when (eq (find-class 'entry) (class-of entry))
	       collect entry)))

    (labels
	((filter-in-account (name account)
	   (declare (ignore name))
	   (setf (account-transactions account)
		 (loop
		    for xact in (account-transactions account)
		    when (eq (find-class 'entry)
			     (class-of (xact-entry xact)))
		    collect xact))
	   (let ((children (account-children account)))
	     (if children
		 (maphash #'filter-in-account children)))))
      (filter-in-account "" (binder-root-account binder)))

    (if (binder-transactions binder)
	(setf (binder-transactions binder)
	      (loop
		 for xact in (binder-transactions binder)
		 when (eq (find-class 'entry)
			  (class-of (xact-entry xact)))
		 collect xact))))
  binder)

(export 'normalize-binder)

(provide 'normalize)
