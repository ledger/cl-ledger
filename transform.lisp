;; transform.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defvar *transformer-keys*
  `(((:total :amount :always) . ,#'calculate-totals)
    ((:account :not-account
	       :payee :not-payee
	       :note :not-note
	       :expr
	       :begin :end) . ,#'apply-filter)
    (:period . ,#'periodic-transform)))

;; jww (2007-11-21): Still need a better way to determine ordering
(defun determine-transforms (args)
  (let (transform-alist)
    (loop
       for arg = args then (cddr arg)
       while arg
       for keyword = (car arg)
       for argument = (cadr arg)
       for function =
       (block nil
	 (mapc #'(lambda (cell)
		   (if (if (keywordp (car cell))
			   (eq keyword (car cell))
			   (member keyword (car cell)))
		       (return (cdr cell))))
	       *transformer-keys*))
       when function do
       (let ((entry (assoc function transform-alist)))
	 (unless entry
	   (push (setf entry (cons function nil))
		 transform-alist))
	 (push argument (cdr entry))
	 (push keyword (cdr entry))))
    
    (mapc #'(lambda (cell)
	      (if (if (keywordp (car cell))
		      (eq :always (car cell))
		      (member :always (car cell)))
		  (unless (assoc (cdr cell) transform-alist)
		    (if transform-alist
			(rplacd (last transform-alist)
				(list (cons (cdr cell) nil)))
			(setf transform-alist
			      (list (cons (cdr cell) nil)))))))
	  *transformer-keys*)

    transform-alist))

(provide 'transform)

;; transform.lisp ends here
