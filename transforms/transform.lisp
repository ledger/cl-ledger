;; transform.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun apply-key-transforms (xacts args)
  (setf xacts (apply #'apply-filter xacts args))

  (let ((period (getf args :period)))
    (when period
      ;; jww (2007-12-01): This should call group-by-period directly, once
      ;; things are working
      (setf xacts (periodic-transform xacts period))))

  (unless (getf args :balance-report)
    (setf xacts (calculate-totals xacts
				  :amount (getf args :amount)
				  :total  (getf args :total)))
    (let (arg)
      (cond
	((setf arg (getf args :head))
	 (setf xacts (subseries xacts 0 arg)))

	((setf arg (getf args :tail))
	 ;; Tail is expensive, because we don't know the length of the
	 ;; series until every element has been seen (and hence computed).
	 ;; Expect a large pause for giant data sets.
	 (setf xacts (subseries 0 (- (collect-length xacts) arg)))))))
  xacts)

(provide 'transform)

;; transform.lisp ends here
