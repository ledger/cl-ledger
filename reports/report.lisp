;; report.lisp

(in-package :ledger)

(defun find-all-transactions (args)
  (multiple-value-bind (current-binder remaining-args)
      (apply #'binder args)
    (values (apply-key-transforms (scan-transactions current-binder)
				  remaining-args)
	    remaining-args)))

(defun basic-reporter (printer args)
  (multiple-value-bind (xacts-or-account plist)
      (find-all-transactions args)
    (apply printer xacts-or-account plist)))

(defun transaction-printer (xact-series &key reporter &allow-other-keys)
  (iterate ((xact xact-series))
    (funcall reporter xact)
    ;; Clear out the extended attribute data so it can be garbage collected
    ;; right away.  For certain reports where a strict SERIES computation is
    ;; possible (i.e., not sorting, reversing, tail, grouping, etc), this
    ;; can reduce committed memory by a significant amount.
    (setf (xact-data xact) nil)))

(defun transactions-report (args)
  (basic-reporter #'transaction-printer args))

(defun print-account (account &key reporter &allow-other-keys)
  (funcall reporter account))

(defun accounts-report (args)
  (basic-reporter #'print-account
		  (append args (list :accounts-report t))))

(provide 'report)

;; report.lisp ends here
