;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defvar *value-expr-observe-properties-p* nil)
(defvar *value-expr-reduce-to-smallest-units-p* nil)
(defvar *value-expr-commodity-pool* nil)

(defparameter *value-expr-readtable* (copy-readtable nil))

(setf (readtable-case *value-expr-readtable*) :preserve)

(defun ignore-character (stream char)
  (declare (ignore stream))
  (declare (ignore char))
  (values))

(dolist (char '(#\& #\|
		#\- #\+ #\* #\/
		#\^ #\? #\:
		#\< #\> #\= #\!
		#\{ #\} #\[ #\] #\( #\)
		#\" #\@ #\;))
  (set-macro-character char #'ignore-character nil *value-expr-readtable*))

(defun read-value-term (in)
  (let ((c (peek-char t in nil))
	found-amount)
    (when c
      (unless (and (not (digit-char-p c))
		   (cambl::symbol-char-invalid-p c))
	(let ((position (file-position in)))
	  (ignore-errors
	    (setf
	     found-amount
	     (read-amount
	      in
	      :observe-properties-p       *value-expr-observe-properties-p*
	      :reduce-to-smallest-units-p *value-expr-reduce-to-smallest-units-p*
	      :pool                       *value-expr-commodity-pool*)))
	  (unless found-amount
	    (file-position in position))))
      (if found-amount
	  (constantly found-amount)
	  (cond
	    ((digit-char-p c)
	     (assert (null "We should never get here anymore"))
	     (constantly (cambl:integer-to-amount (read in))))

	    ((char= c #\/)
	     (read-char in)
	     (let ((scanner
		    (cl-ppcre:create-scanner
		     (cambl::read-until
		      in #\/ "Regular expression lacks closing slash")
		     :case-insensitive-mode t)))
	       (lambda (xact)
		 ;; If just `match' were used here, the result might be 0 if the
		 ;; match occurred at the beginning of the string -- which
		 ;; `value-truth' (applied to the result in filter.lisp) would
		 ;; interpret as FALSE.  By returning T or NIL here, `value-truth'
		 ;; will always do the right thing.
		 (not (null (cl-ppcre:scan
			     scanner (account-fullname (xact-account xact))))))))

	    ((char= c #\()
	     (read-char in)
	     (read-value-expr in :nested-p t))

	    ((char= c #\[)
	     (read-char in)
	     (constantly
	      (parse-datetime
	       (cambl::read-until in #\] "Date/time lacks closing bracket"))))

	    ((char= c #\{)
	     (read-char in)
	     (prog1
		 (constantly
		  (read-amount
		   in
		   :observe-properties-p       *value-expr-observe-properties-p*
		   :reduce-to-smallest-units-p *value-expr-reduce-to-smallest-units-p*
		   :pool                       *value-expr-commodity-pool*))
	       (let ((c (peek-char t in nil)))
		 (if c
		     (if (char= c #\})
			 (read-char in)
			 (error (format nil "Unexpected character '~S'" c)))))))

	    ((alpha-char-p c)
	     (let*
		 ((symbol
		   (let ((*readtable* *value-expr-readtable*)
			 (*package* (find-package :ledger)))
		     (read in)))
		  (function
		   (cond
		     ((member symbol '(|m| |now| |today|) :test #'eq)
		      (constantly (local-time:now)))
		     ((member symbol '(|a| |amount|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-amount xact)))
		     ((member symbol '(|i| |price|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(divide (xact-cost xact) (xact-amount xact))))
		     ((member symbol '(|b| |cost|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-cost xact)))
		     ((member symbol '(|d| |date|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-date xact)))
		     ((member symbol '(|act_date| |actual_date|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-actual-date xact)))
		     ((member symbol '(|eff_date| |effective_date|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-effective-date xact)))
		     ((member symbol '(|X| |cleared|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-cleared-p xact)))
		     ((member symbol '(|Y| |pending|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(xact-pending-p xact)))
		     ((member symbol '(|R| |real|) :test #'eq)
		      )
		     ((member symbol '(|L| |actual|) :test #'eq)
		      )
		     ((member symbol '(|n| |index|) :test #'eq)
		      )
		     ((member symbol '(|N| |count|) :test #'eq)
		      )
		     ((member symbol '(|l| |depth|) :test #'eq)
		      )
		     ((member symbol '(|O| |total|) :test #'eq)
		      )
		     ((member symbol '(|I| |total_price|) :test #'eq)
		      )
		     ((member symbol '(|B| |total_cost|) :test #'eq)
		      )
		     ((eq symbol '|t|)
		      )
		     ((eq symbol '|T|)
		      )
		     ((member symbol '(|U| |abs|) :test #'eq)
		      (lambda (xact value)
			(declare (ignore xact))
			(value-abs value)))
		     ((eq symbol '|round|)
		      (lambda (xact value)
			(declare (ignore xact))
			(value-round value)))
		     ((member symbol '(|S| |quant| |quantity|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(cambl:amount-quantity (xact-amount xact))))
		     ((member symbol '(|comm| |commodity|) :test #'eq)
		      (lambda (xact &rest args)
			(declare (ignore args))
			(cambl:amount-commodity (xact-amount xact))))
		     ((member symbol '(|setcomm| |set_commodity|) :test #'eq)
		      )
		     ((member symbol '(|A| |avg| |mean| |average|) :test #'eq)
		      )
		     ((eq symbol '|P|)
		      )
		     ((member symbol '(|v| |market|) :test #'eq)
		      )
		     ((member symbol '(|V| |total_market|) :test #'eq)
		      )
		     ((member symbol '(|g| |gain|) :test #'eq)
		      )
		     ((member symbol '(|G| |total_gain|) :test #'eq)
		      )
		     (t
		      (let ((symbol
			     (or (find-symbol (format nil "value-expr/~S" symbol))
				 (find-symbol (string-upcase symbol))
				 (error (format nil "Symbol `~S' not found" symbol)))))
			(lambda (xact &rest args)
			  (apply (fdefinition symbol) xact args)))))))

	       (let ((next-char (peek-char t in nil)))
		 (if (and next-char (char= next-char #\())
		     ;; This is a function call
		     (progn
		       (read-char in)
		       (let ((next-function (read-comma-expr in :as-arguments t)))
			 (if next-function
			     (lambda (xact)
			       (apply function xact (funcall next-function)))
			     function)))
		     function)))))))))

(defun read-unary-expr (in)
  (let ((c (peek-char t in nil)))
    (when c
      (cond
	((char= c #\!)
	 (read-char in)
	 (let ((function (read-value-term in)))
	   (if function
	       (lambda (xact)
		 (value-zerop (funcall function xact)))
	       (error "'!' operator not followed by argument"))))
	((char= c #\-)
	 (read-char in)
	 (let ((function (read-value-term in)))
	   (if function
	       (lambda (xact)
		 (cambl:negate (funcall function xact)))
	       (error "'-' operator not followed by argument"))))
	(t
	 (read-value-term in))))))

(defun read-mul-expr (in)
  (let ((function (read-unary-expr in)))
    (when function
      (let ((c (peek-char t in nil)))
	(if c
	    (cond
	      ((char= c #\*)
	       (read-char in)
	       (let ((next-function (read-mul-expr in)))
		 (if next-function
		     (lambda (xact)
		       (multiply (funcall function xact)
				 (funcall next-function xact)))
		     (error "'*' operator not followed by argument"))))
	      ((char= c #\/)
	       (read-char in)
	       (let ((next-function (read-mul-expr in)))
		 (if next-function
		     (lambda (xact)
		       (divide (funcall function xact)
			       (funcall next-function xact)))
		     (error "'/' operator not followed by argument"))))
	      (t
	       function))
	    function)))))

(defun read-add-expr (in)
  (let ((function (read-mul-expr in)))
    (when function
      (let ((c (peek-char t in nil)))
	(if c
	    (cond
	      ((char= c #\+)
	       (read-char in)
	       (let ((next-function (read-add-expr in)))
		 (if next-function
		     (lambda (xact)
		       (add (funcall function xact)
			    (funcall next-function xact)))
		     (error "'+' operator not followed by argument"))))
	      ((char= c #\-)
	       (read-char in)
	       (let ((next-function (read-add-expr in)))
		 (if next-function
		     (lambda (xact)
		       (subtract (funcall function xact)
				 (funcall next-function xact)))
		     (error "'-' operator not followed by argument"))))
	      (t
	       function))
	    function)))))

(defun read-logic-expr (in)
  (let ((function (read-add-expr in)))
    (when function
      (let ((c (peek-char t in nil)))
	(if c
	    (cond
	      ((char= c #\=)
	       (read-char in)
	       (let ((next-function (read-logic-expr in)))
		 (if next-function
		     (lambda (xact)
		       (let ((second (funcall next-function xact)))
			 (and (value= (funcall function xact)
				      second)
			      second)))
		     (error "'=' operator not followed by argument"))))
	      ((char= c #\!)
	       (read-char in)
	       (if (char= #\= (peek-char nil in))
		   (progn
		     (read-char in)
		     (let ((next-function (read-logic-expr in)))
		       (if next-function
			   (lambda (xact)
			     (let ((second (funcall next-function xact)))
			       (and (value/= (funcall function xact)
					     second)
				    second)))
			   (error "'!=' operator not followed by argument"))))
		   (error "Syntax error")))
	      ((char= c #\<)
	       (read-char in)
	       (if (char= #\= (peek-char nil in))
		   (progn
		     (read-char in)
		     (let ((next-function (read-logic-expr in)))
		       (if next-function
			   (lambda (xact)
			     (let ((second (funcall next-function xact)))
			       (and (value<= (funcall function xact)
					     second)
				    second)))
			   (error "'<=' operator not followed by argument"))))
		   (let ((next-function (read-logic-expr in)))
		     (if next-function
			 (lambda (xact)
			   (let ((second (funcall next-function xact)))
			     (and (value< (funcall function xact)
					  second)
				  second)))
			 (error "'<' operator not followed by argument")))))
	      ((char= c #\>)
	       (read-char in)
	       (if (char= #\= (peek-char nil in))
		   (progn
		     (read-char in)
		     (let ((next-function (read-logic-expr in)))
		       (if next-function
			   (lambda (xact)
			     (let ((second (funcall next-function xact)))
			       (and (value>= (funcall function xact)
					     second)
				    second)))
			   (error "'>=' operator not followed by argument"))))
		   (let ((next-function (read-logic-expr in)))
		     (if next-function
			 (lambda (xact)
			   (let ((second (funcall next-function xact)))
			     (and (value> (funcall function xact)
					  second)
				  second)))
			 (error "'>' operator not followed by argument")))))
	      (t
	       function))
	    function)))))

(defun read-and-expr (in)
  (let ((function (read-logic-expr in)))
    (when function
      (let ((c (peek-char t in nil)))
	(if (and c (char= c #\&))
	    (progn
	      (read-char in)
	      (let ((next-function (read-and-expr in)))
		(if next-function
		    (lambda (xact)
		      (and (funcall function xact)
			   (funcall next-function xact)))
		    (error "'&' operator not followed by argument"))))
	    function)))))

(defun read-or-expr (in)
  (let ((function (read-and-expr in)))
    (when function
      (let ((c (peek-char t in nil)))
	(if (and c (char= c #\|))
	    (progn
	      (read-char in)
	      (let ((next-function (read-or-expr in)))
		(if next-function
		    (lambda (xact)
		      (or (funcall function xact)
			  (funcall next-function xact)))
		    (error "'|' operator not followed by argument"))))
	    function)))))

(defun read-comma-expr (in &key (as-arguments nil))
  (let ((function (read-or-expr in)))
    (when function
      (let ((c (peek-char t in nil)))
	(if (and c (char= c #\,))
	    (progn
  	      (read-char in)
	      (let ((next-function (read-comma-expr in)))
		(if next-function
		    (if as-arguments
			(lambda (xact)
			  (let ((next-value (funcall next-function xact)))
			    (cons (funcall function xact)
				  (if (consp next-value)
				      next-value
				      (cons next-value nil)))))
			(lambda (xact)
			  (funcall function xact)
			  (funcall next-function xact)))
		    (error "',' operator not followed by argument"))))
	    function)))))

(defun read-value-expr (in &key
			(observe-properties-p nil)
			(reduce-to-smallest-units-p nil)
			(pool *default-commodity-pool*))
  (let ((*value-expr-observe-properties-p*       observe-properties-p)
	(*value-expr-reduce-to-smallest-units-p* reduce-to-smallest-units-p)
	(*value-expr-commodity-pool*             pool))
    (let ((function (read-comma-expr in)))
      (when function
	(let ((c (peek-char t in nil)))
	  (if c
	      (progn
		(if (char= c #\))
		    (read-char in)
		    (error (format nil "Unexpected character '~S'" c)))
		(lambda (xact)
		  (declare (ignorable xact))
		  (funcall function xact)))
	      function))))))

(export 'read-value-expr)

(defun parse-value-expr (string &key
			 (observe-properties-p nil)
			 (reduce-to-smallest-units-p nil)
			 (pool *default-commodity-pool*))
  (with-input-from-string (in string)
    (read-value-expr in :observe-properties-p observe-properties-p
		     :reduce-to-smallest-units-p reduce-to-smallest-units-p
		     :pool pool)))

(export 'parse-value-expr)

(provide 'valexpr)

;; valexpr.lisp ends here
