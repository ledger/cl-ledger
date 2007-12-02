;; report.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defvar *last-binder* nil)

(defun basic-reporter (printer args)
  (let ((binder *last-binder*))
    (loop while args do
	 (etypecase (first args)
	   (keyword (loop-finish))
	   (binder
	    (setf binder (first args) *last-binder* binder)
	    (loop-finish))
	   ((or string pathname journal)
	    (unless binder
	      (setf binder (make-instance 'binder)
		    *last-binder* binder))
	    (add-journal binder (first args))))
	 (setf args (cdr args)))
    (when binder
      (loop for journal-cell on (binder-journals binder) do
	   (when (or (null (journal-read-date (car journal-cell)))
		     (> (file-write-date (journal-source (car journal-cell)))
			(journal-read-date (car journal-cell))))
	     (format t "Out o' date~%")
	     (setf (car journal-cell)
		   (read-journal binder (journal-source
					 (car journal-cell))))
	     (assert (car journal-cell))))
      (if (binderp binder)
	  (funcall printer (apply-key-transforms
			    (scan-transactions binder) args)
		   :reporter (getf args :reporter)
		   :no-total (getf args :no-total))))
    (values)))

(provide 'report)

;; report.lisp ends here
