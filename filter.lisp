;; filter.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun regex-matcher (regex)
  (declare (type string regex))
  (let ((scanner (cl-ppcre:create-scanner regex :case-insensitive-mode t)))
    (lambda (string)
      (cl-ppcre:scan scanner string))))

(defun eq-matcher (object)
  (lambda (other-object)
    (eq object other-object)))

(defun account-matcher (regex-or-account)
  (declare (type (or string account) regex-or-account))
  (if (stringp regex-or-account)
      (let ((matcher (regex-matcher regex-or-account)))
	(lambda (xact)
	  (funcall matcher (account-fullname (xact-account xact)))))
      (let ((matcher (eq-matcher regex-or-account)))
	(lambda (xact)
	  (funcall matcher (xact-account xact))))))

(defun payee-matcher (regex)
  (declare (type string regex))
  (let ((matcher (regex-matcher regex)))
    (lambda (xact)
      (funcall matcher (entry-payee (xact-entry xact))))))

(defun note-matcher (regex)
  (declare (type string regex))
  (let ((matcher (regex-matcher regex)))
    (lambda (xact)
      (funcall matcher (or (xact-note xact)
			   (entry-note (xact-entry xact)))))))

(defun value-expr-matcher (expr-or-function)
  (declare (type (or string function) expr-or-function))
  (if (stringp expr-or-function)
      (let ((closure (parse-value-expr expr-or-function)))
	(lambda (xact)
	  (funcall closure xact)))
      expr-or-function))

(defun not-matcher (matcher)
  (declare (type function matcher))
  (lambda (xact)
    (not (funcall matcher xact))))

(defun datetime-matcher (string-or-datetime operator)
  (declare (type (or string cambl:datetime) string-or-datetime))
  (if (stringp string-or-datetime)
      (let ((moment (cambl:parse-datetime string-or-datetime)))
	(lambda (xact)
	  (funcall operator (xact-date xact) moment)))
      (lambda (xact)
	(funcall operator (xact-date xact) string-or-datetime))))

(defun compose-predicate (&rest args)
  (if args
      (let* ((first-arg (first args))
	     remainder
	     (function
	      (if (functionp first-arg)
		  (progn
		    (setf remainder (rest args))
		    first-arg)
		  (let ((value (first (rest args))))
		    (setf remainder (rest (rest args)))
		    (when value
		      (case first-arg
			(:account
			 (assert (or (stringp value)
				     (typep value 'account)))
			 (account-matcher value))
			(:not-account
			 (assert (or (stringp value)
				     (typep value 'account)))
			 (not-matcher (account-matcher value)))
			(:payee
			 (assert (stringp value))
			 (payee-matcher value))
			(:not-payee
			 (assert (stringp value))
			 (not-matcher (payee-matcher value)))
			(:note
			 (assert (stringp value))
			 (note-matcher value)) 
			(:not-note
			 (assert (stringp value))
			 (not-matcher (note-matcher value))) 
			(:expr
			 (assert (or (stringp value)
				     (functionp value)))
			 (value-expr-matcher value))
			(:begin
			 (assert (or (stringp value)
				     (typep value 'cambl:datetime)))
			 (datetime-matcher value #'local-time>=))
			(:end
			 (assert (or (stringp value)
				     (typep value 'cambl:datetime)))
			 (datetime-matcher value #'local-time<=))
			(otherwise
			 (error "Unrecognized predicate keyword '~S'"
				first-arg))))))))
	(if remainder
	    (let ((next-matcher (apply #'compose-predicate remainder)))
	      (if next-matcher
		  (if function
		      (lambda (xarg)
			(and (funcall function xarg)
			     (funcall next-matcher xarg)))
		      next-matcher)
		  function))
	    function))
      (constantly t)))

(provide 'filter)

;; filter.lisp ends here
