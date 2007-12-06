;; filter.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun eq-matcher (object)
  (lambda (other-object)
    (eq object other-object)))

(defun regex-matcher (regex)
  (declare (type string regex))
  (let ((scanner (cl-ppcre:create-scanner regex :case-insensitive-mode t)))
    (lambda (string)
      (cl-ppcre:scan scanner string))))

(defun account-matcher (regex-or-account)
  (etypecase regex-or-account
    (function				; a pre-compiled cl-ppcre scanner
     (lambda (xact)
      (funcall regex-or-account (account-fullname (xact-account xact)))))
    (string
     (let ((matcher (regex-matcher regex-or-account)))
       (lambda (xact)
	 (funcall matcher (account-fullname (xact-account xact))))))
    (account
     (let ((matcher (eq-matcher regex-or-account)))
       (lambda (xact)
	 (funcall matcher (xact-account xact)))))))

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
      (let ((closure (value-expr-function
		      (parse-value-expr expr-or-function))))
	(lambda (xact)
	  (funcall closure xact)))
      expr-or-function))

(defun status-matcher (state)
  (declare (type keyword state))
  (lambda (xact)
    (eq (xact-status xact) state)))

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

(defun time-range-matcher (string-or-time-range)
  (declare (type (or string time-range) string-or-time-range))
  (let ((range (if (stringp string-or-time-range)
		   (parse-time-range string-or-time-range)
		   string-or-time-range)))
    (lambda (xact)
      (funcall #'time-within-range-p (xact-date xact) range))))

(defvar *predicate-keywords*
  `((:account
     (or string function account)
     ,#'account-matcher)

    (:not-account
     (or string function account)
     ,#'(lambda (value)
	  (not-matcher (account-matcher value))))

    (:payee
     (or string function)
     ,#'payee-matcher)

    (:not-payee
     (or string function)
     ,#'(lambda (value)
	  (not-matcher (payee-matcher value))))

    (:note
     (or string function)
     ,#'note-matcher)

    (:not-note
     (or string function)
     ,#'(lambda (value)
	  (not-matcher (note-matcher value))))

    (:status
     keyword
     ,#'status-matcher)

    (:not-status
     keyword
     ,#'(lambda (value)
	  (not-matcher (status-matcher value))))

    (:begin
     (or string fixed-time)
     ,#'(lambda (value)
	  (fixed-time-matcher value #'local-time>=)))

    (:end
     (or string fixed-time)
     ,#'(lambda (value)
	  (fixed-time-matcher value #'local-time<=)))

    (:range
     (or string time-range) ,#'time-range-matcher)

    (:expr
     (or string function)
     ,#'value-expr-matcher)
    (:limit
     (or string function)
     ,#'value-expr-matcher))
  "*predicate-keywords* associates keywords that may be passed to
  `apply-filter' or `parse-predicate-keywords' with matcher functions that are
  called to produce the closures used to ascertain the match.

  The format of each member of this list is (KEYWORD TYPE FUNCTION).

  For example, in the case of allowing :ACCOUNT to specify an account to match
  against in a value expression predicate, the require type is either a string
  specifying a regular expression, or an actual account object to compare
  against.  The function used to create the matcher is `account-matcher',
  which takes the string/account argument passed in after the :ACCOUNT
  keyword, and returns a closure which can verify whether a transaction is
  indeed in that account.

  This means that every matcher function takes a value argument to base the
  match on, and returns a closure *that takes a transaction* for which it will
  ascertain that match.")

(defun parse-predicate-keywords (args)
  (let (functions)
    (do ((arg args))
	((null arg))
      (if (keywordp (first arg))
	  (progn
	    (if (first (rest arg))
		(if-let ((entry (assoc (first arg) *predicate-keywords*)))
		  (unless (typep (first (rest arg)) (cadr entry))
		    (error "Argument of invalid type ~S passed to ~
                            predicate keyword ~S (expected ~S)"
			   (type-of (first (rest arg))) (first arg)
			   (cadr entry)))
		  (push (funcall (caddr entry)
				 (first (rest arg))) functions)))
	    (setf arg (cddr arg)))
	  (setf arg (cdr arg))))

    (and functions
	 (lambda (xact)
	   (dolist (predicate functions t)
	     (unless (funcall predicate xact)
	       (return nil)))))))

(declaim (inline apply-filter))
(defun apply-filter (xact-series &rest args)
  (let ((predicate (parse-predicate-keywords args)))
    (if predicate
	(choose-if predicate xact-series)
	xact-series)))

(declaim (inline choose-if-value-expr))
(defun choose-if-value-expr (xact-series expr)
  (choose-if (if (functionp expr) expr
		 (value-expr-function (parse-value-expr expr)))
	     xact-series))

(provide 'filter)

;; filter.lisp ends here
