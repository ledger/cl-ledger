;; register.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun parse-value-term (in)
  (let ((c (peek-char t in nil)))
    (when c
      (cond
	((digit-char-p c)
	 (cambl:integer-to-amount (read in)))
	((char= c #\()
	 (read-char in)
	 (parse-value-expr in))

	((alpha-char-p c)
	 (let* ((symbol
		 (let ((*readtable* (copy-readtable nil))
		       (*package* (find-package :ledger)))
		   (setf (readtable-case *readtable*) :preserve)
		   (read in)))
		(sexp
		 (cond
		   ((member symbol '(|m| |now| |today|) :test #'eq)
		    (get-universal-time))
		   ((member symbol '(|a| |amount|) :test #'eq)
		    '(xact-amount xact))
		   ((member symbol '(|i| |price|) :test #'eq)
		    '(xact-price xact))
		   ((member symbol '(|b| |cost|) :test #'eq)
		    '(xact-cost xact))
		   ((member symbol '(|d| |date|) :test #'eq)
		    '(xact-date xact))
		   ((member symbol '(|act_date| |actual_date|) :test #'eq)
		    '(xact-actual-date xact))
		   ((member symbol '(|eff_date| |effective_date|) :test #'eq)
		    '(xact-effective-date xact))
		   ((member symbol '(|X| |cleared|) :test #'eq)
		    '(xact-cleared-p xact))
		   ((member symbol '(|Y| |pending|) :test #'eq)
		    '(xact-pending-p xact))
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
		    #'value-abs)
		   ((eq symbol '|round|)
		    #'value-round)
		   ((member symbol '(|S| |quant| |quantity|) :test #'eq)
		    '(amount-quantity (xact-amount xact)))
		   ((member symbol '(|comm| |commodity|) :test #'eq)
		    '(amount-commodity (xact-amount xact)))
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
		    (or (find-symbol (format nil "value-expr/~S" symbol))
			(find-symbol (string-upcase symbol))
			(error (format nil "Symbol `~S' not found" symbol)))))))

	   (if (char= (peek-char t in nil) #\()
	       ;; This is a function call
	       (list #'funcall sexp (parse-value-expr in))
	       sexp)))))))

(defun parse-unary-expr (in)
  (parse-value-term in))

(defun parse-mul-expr (in)
  (let ((sexp (parse-unary-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\*)
	     (read-char in)
	     (setf sexp
		   (list 'multiply
			 sexp
			 (or (parse-and-expr in)
			     (error "'*' operator not followed by argument")))))
	    ((char= c #\/)
	     (read-char in)
	     (setf sexp
		   (list 'divide
			 sexp
			 (or (parse-and-expr in)
			     (error "'/' operator not followed by argument")))))))))
    sexp))

(defun parse-add-expr (in)
  (let ((sexp (parse-mul-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\+)
	     (read-char in)
	     (setf sexp
		   (list 'add
			 sexp
			 (or (parse-and-expr in)
			     (error "'+' operator not followed by argument")))))
	    ((char= c #\-)
	     (read-char in)
	     (setf sexp
		   (list 'subtract
			 sexp
			 (or (parse-and-expr in)
			     (error "'-' operator not followed by argument")))))))))
    sexp))

(defun parse-logic-expr (in)
  (let ((sexp (parse-add-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\=)
	     (read-char in)
	     (setf sexp
		   (list 'value=
			 sexp
			 (or (parse-and-expr in)
			     (error "AND operator not followed by argument")))))))))
    sexp))

(defun parse-and-expr (in)
  (let ((sexp (parse-logic-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when (and c (char= c #\&))
	  (read-char in)
	  (setf sexp
		(list 'and
		      sexp
		      (or (parse-and-expr in)
			  (error "AND operator not followed by argument")))))))
    sexp))

(defun parse-or-expr (in)
  (let ((sexp (parse-and-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when (and c (char= c #\|))
	  (read-char in)
	  (setf sexp
		(list 'or
		      sexp
		      (or (parse-or-expr in)
			  (error "OR operator not followed by argument")))))))
    sexp))

(defun parse-comma-expr (in)
  (let ((sexp (parse-or-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\,)
	     (read-char in)
	     (setf sexp
		   (list 'progn
			 sexp
			 (or (parse-comma-expr in)
			     (error "Comma operator not followed by argument")))))
	    ((char= c #\))
	     (read-char in))
	    (t
	     (error (format nil "Unexpected character '~S'" c)))))))
    sexp))

(defun parse-value-expr (in)
  (let ((sexp (parse-comma-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(if c
	    (if (char= c #\))
		(read-char in)
		(error (format nil "Unexpected character '~S'" c))))))
    sexp))

(export 'parse-value-expr)

(provide 'valexpr)

;; valexpr.lisp ends here
