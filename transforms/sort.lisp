;; sort.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun sort-transactions (xact-series
			  &key (key #'xact-amount) (test #'value<))
  (scan (sort (collect xact-series)
	      #'(lambda (left right)
		  (funcall test (funcall key left)
			   (funcall key right))))))

(provide 'sort)

;; sort.lisp ends here
