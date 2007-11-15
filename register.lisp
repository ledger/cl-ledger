;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

;; (put 'do-transactions 'lisp-indent-function 1)

(defun abbreviate-string (name width &key
			  (elision-style 'abbreviate)
			  (account-p nil)
			  (abbrev-length 3))
  (let ((len (length name)))
    (if (<= len width)
	name
	(cond
	  ((eq elision-style 'leading)
	   (concatenate 'string ".." (subseq name (- len (- width 2)))))

	  ((eq elision-style 'middle)
	   (concatenate 'string (subseq name 0 (1- (/ width 2))) ".."
			(subseq name (1+ (/ width 2)))))

	  ((and (eq elision-style 'abbreviate)
		account-p)
	   (let* ((parts (split-string-at-char name #\:))
		  (final-part (car (last parts))) last-part
		  (abbrev
		   (reduce
		    #'(lambda (left right)
			(if (eq right final-part)
			    (concatenate
			     'string
			     (or last-part
				 (subseq left 0 abbrev-length))
			     ":" right)
			    (let ((left-abbrev
				   (or last-part
				       (subseq left 0 abbrev-length)))
				  (right-abbrev
				   (subseq right 0 abbrev-length)))
			      (setf last-part
				    (concatenate 'string left-abbrev ":"
						 right-abbrev))))) parts)))
	     (if (> (length abbrev) width)
		 (abbreviate-string abbrev width :elision-style 'leading)
		 abbrev)))
	  (t
	   (concatenate 'string (subseq name 0 (- width 2)) ".."))))))

(export 'abbreviate-string)

(defun register-report (binder &key (output-stream *standard-output*))
  (declare (type binder binder))
  (declare (type stream output-stream))
  (let ((count 0))
   (do-transactions (xact binder)
     (format output-stream "~10A ~20A ~22A ~A ~A~%"
	     (cambl:format-datetime (xact-date xact))
	     (abbreviate-string (entry-payee (xact-entry xact)) 20)
	     (abbreviate-string (account-fullname (xact-account xact)) 22
				:account-p t)
	     (format-value (xact-amount xact)
			   :width 12 :latter-width 67)
	     (let ((running-total (xact-value :running-total xact)))
	       (if running-total
		   (format-value running-total
				 :width 12 :latter-width 80)
		   "")))
     (incf count))
   count))

(export 'register-report)

(defun register (binder &rest args)
  "This is a convenience function for quickly making register reports.

  A typical usage might be:

    (ledger:register \"/path/to/ledger.dat\"
                     :begin \"2007/08/26\" :account \"food\")

  "
  (let* ((binder-object
	  (etypecase binder
	    ((or string pathname) (read-binder binder))
	    (binder binder)))
	 (normalized-binder (normalize-binder binder-object)))
    (register-report
     (calculate-totals
      (if args
	  (destructively-filter normalized-binder
				(apply #'compose-predicate args))
	  normalized-binder)))))

(export 'register)

(provide 'register)

;; register.lisp ends here
