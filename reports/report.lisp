;; report.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun basic-reporter (printer args)
  (let ((binder (typecase (first args)
		  ((or string pathname)
		   (prog1
		       (read-binder (first args))
		     (setf args (rest args))))
		  (binder
		   (prog1
		       (first args)
		     (setf args (rest args))))
		  (otherwise *last-binder*))))
    (if (binderp binder)
	(funcall printer (apply-key-transforms
			  (scan-normalized-transactions binder) args)
		 :reporter (getf args :reporter)))
    (values)))

(provide 'report)

;; report.lisp ends here
