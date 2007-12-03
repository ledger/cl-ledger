;; general.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

;;;_ * General utility functions

(defmacro if-let (((var value)) &body body)
  `(let ((,var ,value))
     (if ,var
	 (progn ,@body))))

(defun split-string-at-char (string char)
  "Returns a list of substrings of string
divided by ONE char each.
Note: Two consecutive chars will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defmacro while (test-form &body body)
  `(do () ((not ,test-form))
     ,@body))

(declaim (inline ignore-args))
(defun ignore-args (function)
  (lambda (&rest args)
    (declare (ignore args))
    (funcall function)))

(declaim (inline ignore-rest))
(defun ignore-rest (function)
  (lambda (arg &rest args)
    (declare (ignore args))
    (funcall function arg)))

(declaim (inline list-iterator))
(defun list-iterator (list)
  (lambda ()
    (prog1
	(first list)
      (setf list (rest list)))))

(declaim (inline map-iterator))
(defun map-iterator (callable iterator)
  ;; This makes the assumption that iterator returns NIL when it reaches end
  ;; of series.
  (loop
     for value = (funcall iterator)
     while value do (funcall callable value)))

(defmacro do-iterator ((var iterator &optional (result nil)) &body body)
  `(block nil
     (map-iterator #'(lambda (,var) ,@body) ,iterator)
     ,result))

(defun chain-functions (first-arg &rest args)
  "Call a group of functions by chaining, passing all keyword args.

  This function allows you to call a set of functions like this:

    (chain-functions arg #'foo :foo 10 :foo2 20
                         #'bar :bar 30)

  This is equivalent to:

    (bar (foo arg :foo 10 :foo2 20) :bar 30)"
  (if args
      (apply
       #'chain-functions
       (apply (first args)
	      (cons first-arg
		    (when (rest args)
		      (setf args (rest args))
		      (loop
			 while (keywordp (first args))
			 collect (first args)
			 collect (first (rest args))
			 do (setf args (rest (rest args)))))))
       args)
      first-arg))

(provide 'general)

;; general.lisp ends here
