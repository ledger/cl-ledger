;; sort.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun sort-entries (xact-series &key (key #'xact-amount) (test #'value<))
  (let ((g (gatherer #'collect))
	last-entry entry-xacts)
    (iterate ((xact xact-series))
      (when last-entry
	(unless (eq last-entry (xact-entry xact))
	  (dolist (x (sort entry-xacts test :key key))
	    (next-out g x))
	  (setf entry-xacts nil)))
      (setf last-entry (xact-entry xact))
      (push xact entry-xacts))
    (if entry-xacts
	(dolist (x (sort entry-xacts test :key key))
	  (next-out g x)))
    (scan (result-of g))))

(defun sort-transactions (xact-series &key (key #'xact-amount) (test #'value<))
  (scan (sort (collect xact-series)
	      #'(lambda (left right)
		  (funcall test (funcall key left)
			   (funcall key right))))))

(provide 'sort)

;; sort.lisp ends here
