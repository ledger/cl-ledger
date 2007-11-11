;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))

(in-package :ledger)

(defun report (&rest args)
  (let (binder-args keyword-args)
    (loop
       for arg = args then (cdr arg)
       for value = (car arg)
       while arg
       do (if (or (typep value 'binder)
		  (typep value 'string)
		  (typep value 'pathname))
	      (push value binder-args)
	      (progn
		(setf keyword-args arg)
		(return))))
    (let ((binder (apply #'binder binder-args)))
      (loop
	 while keyword-args
	 do
	 (if (functionp (car keyword-args))
	     (let ((function (car keyword-args))
		   keywords)
	       (setf keyword-args (cdr keyword-args))
	       (loop
		  while keyword-args
		  until (functionp (car keyword-args))
		  do
		  (push (car keyword-args) keywords)
		  (setf keyword-args (cdr keyword-args)))
	       (setf binder (apply function
				   (append (list binder) keywords))))
	     (setf keyword-args (cdr keyword-args)))))))

(defun register (&rest args)
  (apply #'report
	 (append args
		 (list #'normalize-binder
		       #'calculate-totals
		       #'register-report))))

(export 'report)
(export 'register)

;; register.lisp ends here
