;; transform.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun apply-key-transforms (xacts &rest args)
  (setf xacts (apply #'apply-filter xacts args))
  (setf xacts (calculate-totals xacts
				:amount (getf args :amount)
				:total  (getf args :total)))
  (let (arg)
   (cond
     ((setf arg (getf args :head))
      (setf xacts (choose (latch xacts :after arg) xacts)))

     ((setf arg (getf args :tail))
      ;; Tail is expensive, because we don't know the length of the
      ;; series until every element has been seen (and hence computed).
      ;; Expect a large pause for giant data sets.
      (setf xacts (choose (latch xacts :after (- (collect-length xacts)
						 arg)
				       :pre nil) xacts)))))
  xacts)

(provide 'transform)

;; transform.lisp ends here
