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

(defun longest (&rest items)
  (let (current-length)
    (reduce #'(lambda (left right)
		(let ((left-len  (or current-length
				     (length left)))
		      (right-len (length right)))
		  (if (< left-len right-len)
		      (prog1
			  right
			(setf current-length right-len))
		      (prog1
			  left
			(setf current-length left-len)))))
	    items)))

(defun register (&rest args)
  (let ((filter-keyword
	 (member-if
	  #'(lambda (element)
	      (member element '(:account :payee :note :expr))) args))
	dont-normalize)
    (when filter-keyword
      (rplacd filter-keyword
	      (append (list #'destructively-filter)
		      (cons (car filter-keyword)
			    (cdr filter-keyword))))
      (rplaca filter-keyword #'normalize-binder)
      (setf dont-normalize t))
    (apply #'report
	   (append args
		   (unless dont-normalize
		     (list #'normalize-binder))
		   (list #'calculate-totals
			 #'register-report)))))

(export 'report)
(export 'register)

(provide 'report)

;; register.lisp ends here
