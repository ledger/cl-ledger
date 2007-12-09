;; report.lisp

(in-package :ledger)

(defun find-all-transactions (args)
  (multiple-value-bind (current-binder remaining-args)
      (apply #'binder args)
    (values (apply-key-transforms (scan-transactions current-binder)
				  remaining-args)
	    remaining-args)))

(defun basic-reporter (printer args)
  (multiple-value-bind (xact-series plist) (find-all-transactions args)
    (apply printer xact-series plist)))

(provide 'report)

;; report.lisp ends here
