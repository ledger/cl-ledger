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

(dolist (char '(#\;
		#\- #\+
		#\* #\/
		#\^ #\? #\:
		#\& #\|
		#\!
		#\=
		#\"
		#\< #\>
		#\{ #\}
		#\[ #\]
		#\( #\)
		#\@
		))
  (set-macro-character char #'ignore-character nil *value-expr-readtable*))

(defun read-value-term (in)
  (let ((c (peek-char t in nil))
	found-amount)
    (when c
      (unless (cambl::symbol-char-invalid-p c)
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
      (or
       found-amount
       (cond
	 ((or (char= c '#\-)
	      (digit-char-p c))
	  (cambl:integer-to-amount (read in)))

	 ((char= c #\/)
	  (read-char in)
	  `(let ((match
		  (cl-ppcre:scan
		   ,(cl-ppcre:create-scanner
		     (cambl::read-until in #\/
					"Regular expression lacks closing slash")
		     :case-insensitive-mode t)
		   (account-fullname (xact-account xact)))))
	     ;; If just `match' were used here, the result might be 0 if the
	     ;; match occurred at the beginning of the string -- which
	     ;; `value-truth' (applied to the result in filter.lisp) would
	     ;; interpret as FALSE.  By returning T or NIL here, `value-truth'
	     ;; will always do the right thing.
	     (not (null match))))

	 ((char= c #\()
	  (read-char in)
	  (read-value-expr in :nested-p t))

	 ((char= c #\[)
	  ;; jww (2007-11-11): Read in a local date/time here
	  )

	 ((char= c #\{)
	  (read-char in)
	  (prog1
	      (read-amount
	       in
	       :observe-properties-p       *value-expr-observe-properties-p*
	       :reduce-to-smallest-units-p *value-expr-reduce-to-smallest-units-p*
	       :pool                       *value-expr-commodity-pool*)
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
		   'value-abs)
		  ((eq symbol '|round|)
		   'value-round)
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
		(progn
		  (read-char in)
		  (list sexp (read-value-expr in :nested-p t)))
		sexp))))))))

(defun read-unary-expr (in)
  (let ((c (peek-char t in nil)))
    (when c
      (cond
	((char= c #\!)
	 (read-char in)
	 (list 'not
	       (list 'value-truth
		     (or (read-value-term in)
			 (error "'!' operator not followed by argument")))))
	((char= c #\-)
	 (read-char in)
	 (list 'negate
	       (or (read-value-term in)
		   (error "'-' unary operator not followed by argument"))))
	(t
	 (read-value-term in))))))

(defun read-mul-expr (in)
  (let ((sexp (read-unary-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\*)
	     (read-char in)
	     (setf sexp
		   (list 'multiply
			 sexp
			 (or (read-unary-expr in)
			     (error "'*' operator not followed by argument")))))
	    ((char= c #\/)
	     (read-char in)
	     (setf sexp
		   (list 'divide
			 sexp
			 (or (read-unary-expr in)
			     (error "'/' operator not followed by argument")))))))))
    sexp))

(defun read-add-expr (in)
  (let ((sexp (read-mul-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\+)
	     (read-char in)
	     (setf sexp
		   (list 'add
			 sexp
			 (or (read-mul-expr in)
			     (error "'+' operator not followed by argument")))))
	    ((char= c #\-)
	     (read-char in)
	     (setf sexp
		   (list 'subtract
			 sexp
			 (or (read-mul-expr in)
			     (error "'-' operator not followed by argument")))))))))
    sexp))

(defun read-logic-expr (in)
  (let ((sexp (read-add-expr in)))
    (when sexp
      (let ((c (peek-char t in nil)))
	(when c
	  (cond
	    ((char= c #\=)
	     (read-char in)
	     (setf sexp
		   (list 'value=
			 sexp
			 (or (read-add-expr in)
			     (error "'=' operator not followed by argument")))))
	    ((char= c #\!)
	     (read-char in)
	     (if (char= #\= (peek-char nil in))
		 (progn
		   (read-char in)
		   (setf sexp
			 (list 'value/=
			       sexp
			       (or (read-add-expr in)
				   (error "'!=' operator not followed by argument")))))
		 (error "Syntax error")))
	    ((char= c #\<)
	     (read-char in)
	     (if (char= #\= (peek-char nil in))
		 (progn
		   (read-char in)
		   (setf sexp
			 (list 'value<=
			       sexp
			       (or (read-add-expr in)
				   (error "'<=' operator not followed by argument")))))
		 (setf sexp
		       (list 'value<
			     sexp
			     (or (read-add-expr in)
				 (error "'<' operator not followed by argument"))))))
	    ((char= c #\>)
	     (read-char in)
	     (if (char= #\= (peek-char nil in))
		 (progn
		   (read-char in)
		   (setf sexp
			 (list 'value>=
			       sexp
			       (or (read-add-expr in)
				   (error "'>=' operator not followed by argument")))))
		 (setf sexp
		       (list 'value>
			     sexp
			     (or (read-add-expr in)
				 (error "'>' operator not followed by argument"))))))))))
    sexp))

(defun read-and-expr (in)
  (let ((sexp (read-logic-expr in)))
    (when sexp
      (let ((next-sexps
	     (loop
		for c = (peek-char t in nil)
		while (and c (char= c #\&))
		collect
		(or (read-comma-expr in)
		    (error "'&' operator not followed by argument")))))
	(if next-sexps
	    `(and ,sexp ,@next-sexps)
	    sexp)))))

(defun read-or-expr (in)
  (let ((sexp (read-and-expr in)))
    (when sexp
      (let ((next-sexps
	     (loop
		for c = (peek-char t in nil)
		while (and c (char= c #\|))
		collect
		(or (read-comma-expr in)
		    (error "'|' operator not followed by argument")))))
	(if next-sexps
	    `(or ,sexp ,@next-sexps)
	    sexp)))))

(defun read-comma-expr (in)
  (let ((sexp (read-or-expr in)))
    (when sexp
      (let ((next-sexps
	     (loop
		for c = (peek-char t in nil)
		while (and c (char= c #\,))
		collect
		(or (read-comma-expr in)
		    (error "',' operator not followed by argument")))))
	(if next-sexps
	    `(progn ,sexp ,@next-sexps)
	    sexp)))))

(defun read-value-expr (in &key
			(observe-properties-p nil)
			(reduce-to-smallest-units-p nil)
			(pool *default-commodity-pool*)
			(nested-p nil))
  (let ((*value-expr-observe-properties-p*
	 (if nested-p
	     *value-expr-observe-properties-p*
	     observe-properties-p))
	(*value-expr-reduce-to-smallest-units-p*
	 (if nested-p
	     *value-expr-reduce-to-smallest-units-p*
	     reduce-to-smallest-units-p))
	(*value-expr-commodity-pool*
	 (if nested-p
	     *value-expr-commodity-pool*
	     pool)))
    (let ((sexp (read-comma-expr in)))
      (when sexp
	(let ((c (peek-char t in nil)))
	  (if c
	      (if (char= c #\))
		  (read-char in)
		  (error (format nil "Unexpected character '~S'" c))))))
      `(lambda (xact)
	 (declare (ignorable xact))
	 ,sexp))))

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

(defun compile-value-expr (string &key
			   (observe-properties-p nil)
			   (reduce-to-smallest-units-p nil)
			   (pool *default-commodity-pool*))
  (with-input-from-string (in string)
    (compile
     nil
     (read-value-expr in :observe-properties-p observe-properties-p
			 :reduce-to-smallest-units-p reduce-to-smallest-units-p
			 :pool pool))))

(export 'compile-value-expr)

(provide 'valexpr)

;; valexpr.lisp ends here
