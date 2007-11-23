;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

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
						 right-abbrev)))))
		    parts)))
	     (if (> (length abbrev) width)
		 (abbreviate-string abbrev width :elision-style 'leading)
		 abbrev)))
	  (t
	   (concatenate 'string (subseq name 0 (- width 2)) ".."))))))

(defun register-reporter (&key (output-stream *standard-output*))
  (let (last-entry)
    (lambda (xact)
      (if (or (null last-entry)
	      (not (eq last-entry (xact-entry xact))))
	  (progn
	    (format output-stream "~10A ~20A "
		    ;; jww (2007-11-19): What if the transaction has its own date
		    ;; set?
		    (strftime (xact-date xact))
		    (abbreviate-string (entry-payee (xact-entry xact)) 20))
	    (setf last-entry (xact-entry xact)))
	  (format output-stream "~32A" " "))
      (format output-stream "~22A ~A ~A~%"
	      (abbreviate-string (account-fullname (xact-account xact)) 22
				 :account-p t)
	      (format-value (xact-resolve-amount xact)
			    :width 12 :latter-width 67)
	      (format-value (or (xact-value xact :running-total)
				0) :width 12 :latter-width 80)))))

(defun print-register (xact-series &key (reporter nil))
  (let ((reporter (or reporter (register-reporter)))
	(count 0))
    (iterate ((xact xact-series))
	     (funcall reporter xact)
	     (incf count))
    count))

(defun register-report (&rest args)
  "This is a convenience function for quickly making register reports.

  A typical usage might be:

    (ledger:register-report \"/path/to/ledger.dat\"
                            :begin \"2007/08/26\" :account \"food\")"
  (basic-reporter #'print-register args))

(provide 'register)

;; register.lisp ends here
