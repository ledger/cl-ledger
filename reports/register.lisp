;; register.lisp

(in-package :ledger)

(defun maybe-subseq (str idx &optional len)
  (if (< (if len (+ idx len) idx)
	 (length str))
      (subseq str idx len)
      str))

(defun abbreviate-string (name width &key (elision-style 'abbreviate)
			  (account-p nil) (abbrev-length 3))
  (let ((len (length name)))
    (if (<= len width)
	name
	(cond
	  ((eq elision-style 'leading)
	   (concatenate 'string ".." (subseq name (- len (- width 2)))))

	  ((eq elision-style 'trailing)
	   (concatenate 'string (subseq name 0 (- width 2)) ".."))

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
				 (maybe-subseq left 0 abbrev-length))
			     ":" right)
			    (let ((left-abbrev
				   (or last-part
				       (maybe-subseq left 0 abbrev-length)))
				  (right-abbrev
				   (maybe-subseq right 0 abbrev-length)))
			      (setf last-part
				    (concatenate 'string left-abbrev ":"
						 right-abbrev)))))
		    parts)))
	     (if (> (length abbrev) width)
		 (abbreviate-string abbrev width :elision-style 'middle)
		 abbrev)))
	  (t
	   (concatenate 'string (maybe-subseq name 0 (1- (floor width 2))) ".."
			(maybe-subseq name (1+ (- len (floor width 2))))))))))

(defun register-reporter (&optional (output-stream *standard-output*))
  (let (last-entry)
    (lambda (xact)
      ;; First display the entry details, if it would not be repeated
      (if (or (null last-entry)
	      (not (eq last-entry (xact-entry xact))))
          (let* ((xact-date (strftime (xact-date xact)))
                 (payee-length (- 30 (length xact-date))))
            (format output-stream "~A ~vA "
                    xact-date
                    payee-length
                    (abbreviate-string (entry-payee (xact-entry xact))
                                       payee-length))
	    (setf last-entry (xact-entry xact)))
	  (format output-stream "~32A" " "))

      ;; Then display the transaction details
      (format output-stream "~22A ~A ~A~%"
	      (let ((account (xact-account xact)))
		(if account
		    (abbreviate-string (account-fullname account) 22
				       :account-p t)
		    ""))
	      (let ((amount (xact-display-amount xact)))
		(if amount
		    (format-value amount :width 12 :latter-width 67)
		    ""))
	      (let ((total (xact-display-total xact)))
		(if total
		    (format-value total :width 12 :latter-width 80)
		    ""))))))

(defun register-report (&rest args)
  "This is a function for easily print register reports.

  A typical usage might be:

    (ledger:register-report \"/path/to/ledger.dat\"
                            :begin \"2007/08/26\" :account \"food\")"
  (let ((output-stream (member :output-stream args)))
    (transactions-report
     (append args (list :reporter
			(register-reporter (if output-stream
					       (cadr output-stream)
					       *standard-output*)))))))

(provide 'register)

;; register.lisp ends here
