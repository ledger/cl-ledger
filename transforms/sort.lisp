;; sort.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun sort-entries (xact-series &key (test #'value<) (key #'xact-amount))
  (scan-lists-of-lists-fringe
   (mapcar #'(lambda (entry-xacts)
	       (sort entry-xacts test :key key))
	   (group-transactions-by-entry (collect xact-series)))))

(defun sort-transactions (xact-series &key (test #'value<) (key #'xact-amount))
  (scan (sort (collect xact-series) test :key key)))

(provide 'sort)

;; sort.lisp ends here
