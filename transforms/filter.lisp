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

(defun fixed-time-matcher (string-or-fixed-time operator)
  (declare (type (or string fixed-time) string-or-fixed-time))
  (if (stringp string-or-fixed-time)
      (let ((moment (strptime string-or-fixed-time)))
	(lambda (xact)
	  (funcall operator (xact-date xact) moment)))
      (lambda (xact)
	(funcall operator (xact-date xact) string-or-fixed-time))))

(defun compose-predicate (&rest args)
  (let (function)
    (do ((arg args))
	((null arg))
      (let ((next-function
	     (cond
	       ((keywordp (first arg))
		(let ((value (car (rest arg))))
		  (prog1
		      (case (first arg)
			(:account
			 (if (or (stringp value)
				 (typep value 'account))
			     (account-matcher value)))
			(:not-account
			 (if (or (stringp value)
				 (typep value 'account))
			     (not-matcher (account-matcher value))))
			(:payee
			 (if (stringp value)
			     (payee-matcher value)))
			(:not-payee
			 (if (stringp value)
			     (not-matcher (payee-matcher value))))
			(:note
			 (if (stringp value)
			     (note-matcher value))) 
			(:not-note
			 (if (stringp value)
			     (not-matcher (note-matcher value)))) 
			(:expr
			 (if (or (stringp value)
				 (functionp value))
			     (value-expr-matcher value)))
			(:begin
			 (if (or (stringp value)
				 (typep value 'fixed-time))
			     (fixed-time-matcher value #'local-time>=)))
			(:end
			 (if (or (stringp value)
				 (typep value 'fixed-time))
			     (fixed-time-matcher value #'local-time<=)))
			(otherwise
			 (error "Unrecognized predicate keyword '~S'"
				(first arg))))
		    (setf arg (rest (rest arg))))))

	       ((functionp (car arg))
		(prog1
		    (car arg)
		 (setf arg (rest arg))))

	       (t
		(setf arg (rest arg))
		nil))))

	(when next-function
	  (setf function
		(if function
		    (lambda (xarg)
		      (and (funcall function xarg)
			   (funcall next-function xarg)))
		    next-function)))))

    (or function (constantly t))))

(declaim (inline apply-filter))
(defun apply-filter (xacts &rest args)
  (choose-if (apply #'compose-predicate args) xacts))

(provide 'filter)

;; filter.lisp ends here
