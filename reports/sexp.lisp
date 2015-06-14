;; sexp.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun xacts-to-sexp (entry-xacts)
  (let ((entry (xact-entry (first entry-xacts))))
    (list
     (item-position-begin-line (entry-position entry))
     (append (multiple-value-list
	      (floor (timestamp-to-unix (entry-date entry))
		     65536))
	     (list 0))
     (entry-status entry)
     (entry-payee entry)
     (collect
	 (map-fn 'list
		 #'(lambda (xact)
		     (list (item-position-begin-line (xact-position xact))
			   (xact-status xact)
			   (account-fullname (xact-account xact))
			   (format-value (xact-amount xact))
			   (format-value
			    (or (xact-value xact :running-total) 0))))
		 (choose-if
		  #'(lambda (xact)
		      (not (null (xact-position xact))))
		  (scan entry-xacts)))))))

(defun transactions-to-sexp (xact-series &key (at-line nil) &allow-other-keys)
  (if at-line
      (list
       (xacts-to-sexp
	(collect
	    (choose-if
	     #'(lambda (xact)
		 (let* ((entry (xact-entry xact))
			(start-line (and (entry-position entry)
					 (item-position-begin-line
					  (entry-position entry)))))
		   (and start-line
			(>= at-line start-line)
			(< at-line (+ start-line
				      (length (entry-transactions entry)))))))
	     xact-series))))
      (mapcar #'xacts-to-sexp
	      (group-transactions-by-entry (collect xact-series)))))

(defun sexp-report (&rest args)
  (basic-reporter #'transactions-to-sexp args))

(defun find-unique-payees (&rest args)
  (multiple-value-bind (binder remaining-args) (apply #'binder args)
    (let* ((starts-with (getf remaining-args :starts-with))
	   (starts-with-len (and starts-with (length starts-with))))
      (if starts-with
	  (setf starts-with (string-upcase starts-with)))
      (nreverse
       (remove-duplicates
	(collect (choose-if
		  #'(lambda (payee)
		      (or (null starts-with)
			  (if (>= (length payee) starts-with-len)
			      (string= starts-with
				       (string-upcase
					(subseq payee 0 starts-with-len))))))
		  (map-fn 'string #'entry-payee (scan-entries binder))))
	:test #'string=)))))

(defun find-account-tree (&rest args)
  (let* ((binder (apply #'binder args))
	 (root-account (binder-root-account binder)))
    (labels
	((find-accounts (account)
	   (cons
	    (account-name account)
	    (let ((children (account-children account))
		  child-accounts)
	      (when children
		(maphash #'(lambda (name account)
			     (declare (ignore name))
			     (push (find-accounts account)
				   child-accounts))
			 children))
	      child-accounts))))
      (cdr (find-accounts root-account)))))

(defun find-sibling-accounts (&rest args)
  (multiple-value-bind (binder remaining-args) (apply #'binder args)
    (labels
	((traverse-accounts (account path-elements)
	   (if account
	       (if path-elements
		   (traverse-accounts
		    (find-child-account account (car path-elements))
		    (cdr path-elements))
		   (multiple-value-call
		       #'map-fn 'string
		       #'(lambda (name account)
			   (if (account-children account)
			       (concatenate 'string name ":")
			       name))
		       (scan-hash
			(account-children account)))))))
      (collect (traverse-accounts
		(binder-root-account binder)
		(nbutlast (split-string-at-char
			   (getf remaining-args :path) #\:)))))))

(provide 'sexp)

;; sexp.lisp ends here
