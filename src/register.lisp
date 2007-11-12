;; register.lisp

(declaim (optimize (safety 3) (debug 3)))

(in-package :ledger)

;; (put 'do-transactions 'lisp-indent-function 1)

(defun longest (&rest items)
  (let (current-length)
    (reduce #'(lambda (left right)
		(let ((left-len  (or current-length
				     (length left)))
		      (right-len (length right)))
		  (if (< left-len right-len)
		      (prog1
			  right
			(setf current-length right-len))
		      (prog1
			  left
			(setf current-length left-len)))))
	    items)))

(defun abbreviate-string (name width &key
			  (elision-style 'abbreviate)
			  (account-p nil)
			  (abbrev-length 3))
  (let ((len (length name)))
    (if (<= len width)
	name
	(cond
	  ((eq elision-style 'leading)
	   (concatenate 'string ".." (subseq name (- len width))))

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
  (do-transactions (xact binder)
    (format output-stream "~10A ~20A ~22A ~A ~A~%"
	    "DATE"
	    (abbreviate-string (entry-payee (xact-entry xact)) 20)
	    (abbreviate-string (account-fullname (xact-account xact)) 22
			       :account-p t)
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
