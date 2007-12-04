;; report.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defvar *last-binder* nil)

(defun compare-path-lists (left right)
  (dolist (l left)
    (dolist (r right)
      (unless (equal l r)
	(return-from compare-path-lists nil))))
  t)

(defun find-all-transactions (args)
  (let (binder objects)
    (loop while args do
	 (etypecase (first args)
	   (keyword (loop-finish))
	   (binder
	    (setf binder (first args))
	    (dolist (journal (binder-journals binder))
	      (push journal objects)))
	   ((or string pathname journal)
	    (push (first args) objects)))
	 (setf args (cdr args)))

    (if binder
	(setf *last-binder* nil)
	(setf binder *last-binder*))

    (unless (and *last-binder*
		 (compare-path-lists
		  (mapcar #'(lambda (obj)
			      (etypecase obj
				(journal (journal-source obj))
				(pathname obj)
				(string (pathname obj))))
			  objects)
		  (mapcar #'journal-source
			  (binder-journals *last-binder*))))
      (setf binder (make-instance 'binder))
      (dolist (object objects)
	(add-journal binder object)))

    (when (and binder (binderp binder))
      (setf *last-binder* binder)

      (loop for journal-cell on (binder-journals binder) do
	   (when (or (null (journal-read-date (car journal-cell)))
		     (> (file-write-date (journal-source (car journal-cell)))
			(journal-read-date (car journal-cell))))
	     (setf (car journal-cell)
		   (read-journal (journal-source
				  (car journal-cell)) binder))
	     (assert (car journal-cell))))

      (values (apply-key-transforms (scan-transactions binder) args)
	      args))))

(defun basic-reporter (printer args)
  (multiple-value-bind (xact-series plist)
      (find-all-transactions args)
    (apply printer xact-series plist)))

(provide 'report)

;; report.lisp ends here
