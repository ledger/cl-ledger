;; general.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

;;;_ * General utility functions

(defmacro if-let (((var value)) &body body)
  `(let ((,var ,value))
     (if ,var
	 (progn ,@body))))

(defmacro do-recurse (name (&rest params) &body body)
  "A simple macro for writing inline recursive code.

  (do-recurse this ((x 10))
    (if (= x 0)
        0
        (+ x (this (1- x))))) => 55"
  `(labels ((,name (,@(mapcar #'first params))
	      ,@body))
     (,name ,@(mapcar #'second params))))

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
  (lambda () (pop list)))

(declaim (inline map-iterator))
(defun map-iterator (callable iterator)
  ;; This makes the assumption that iterator returns NIL when it reaches the
  ;; end.
  (loop for value = (funcall iterator) while value do
       (funcall callable value)))

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
      (apply #'chain-functions
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

(defmacro add-to-plist (plist key value)
  `(if (getf ,plist ,key)
       (setf (getf ,plist ,key) ,value)
       (progn
	 (push ,value ,plist)
	 (push ,key ,plist))))

(provide 'general)

;; general.lisp ends here
