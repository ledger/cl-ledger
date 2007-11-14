;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'normalize)
  (require 'totals)
  (require 'filter)
  (require 'register))

(defmacro while (test-form &body body)
  `(do () ((not ,test-form))
     ,@body))

(defun report (&rest args)
  (let (binder-args keyword-args)
    (do* ((arg args (cdr arg))
	  (value (car arg) (car arg)))
	 ((null arg))
      (if (or (typep value 'binder)
	      (typep value 'string)
	      (typep value 'pathname))
	  (push value binder-args)
	  (progn
	    (setf keyword-args arg)
	    (return))))
    (let ((binder (apply #'binder binder-args)))
      (while keyword-args
	(if (functionp (car keyword-args))
	    (setf binder
		  (apply (car keyword-args)
			 (append
			  (list binder)
			  (loop
			     do (setf keyword-args (cdr keyword-args))
			     while (and keyword-args
					(not (functionp (car keyword-args))))
			     collect (car keyword-args)))))
	    (setf keyword-args (cdr keyword-args))))
      binder)))

(export 'report)

(provide 'report)

;; register.lisp ends here
