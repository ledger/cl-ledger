;; sort.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun sort-entries (xact-series &key (test #'value<) (key #'xact-amount))
  (scan-lists-of-lists-fringe
   (mapcar #'(lambda (entry-xacts)
	       (sort entry-xacts test :key key))
	   (nreverse
	    (reduce #'(lambda (entries xact)
			(if entries
			    (if (eq (xact-entry (caar entries))
				    (xact-entry xact))
				(cons (cons xact (first entries))
				      (rest entries))
				(cons (list xact) entries))
			    (list (list xact))))
		    (collect xact-series)
		    :initial-value '())))))

(defun sort-transactions (xact-series &key (test #'value<) (key #'xact-amount))
  (scan (sort (collect xact-series) test :key key)))

(provide 'sort)

;; sort.lisp ends here
