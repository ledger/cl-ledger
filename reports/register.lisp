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

(defun register-reporter (&key (output-stream *standard-output*)
			  (no-total nil))
  (let (last-entry)
    (lambda (xact)
      ;; First display the entry details, if it would not be repeated
      (if (or (null last-entry)
	      (not (eq last-entry (xact-entry xact))))
	  (progn
	    (format output-stream "~10A ~20A " (strftime (xact-date xact))
		    (abbreviate-string (entry-payee (xact-entry xact)) 20))
	    (setf last-entry (xact-entry xact)))
	  (format output-stream "~32A" " "))

      ;; Then display the transaction details
      (format output-stream "~22A ~A ~A~%"
	      (abbreviate-string (account-fullname (xact-account xact)) 22
				 :account-p t)
	      (format-value (xact-amount xact)
			    :width 12 :latter-width 67)
	      (if no-total ""
		  (format-value (or (xact-value xact :running-total)
				    0) :width 12 :latter-width 80))))))

(defun print-register (xact-series &key (reporter nil) (no-total nil)
		       (output-stream *standard-output*) &allow-other-keys)
  (let ((reporter (or reporter
		      (register-reporter :no-total no-total
					 :output-stream output-stream))))
    (iterate ((xact xact-series))
      (funcall reporter xact))))

(defun register-report (&rest args)
  "This is a function for easily print register reports.

  A typical usage might be:

    (ledger:register-report \"/path/to/ledger.dat\"
                            :begin \"2007/08/26\" :account \"food\")"
  (basic-reporter #'print-register args))

(provide 'register)

;; register.lisp ends here
