;;; driver.lisp -- A command-line driver for CL-Ledger

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun regexp-opt (exprs)
  (when exprs
    (if (= (length exprs) 1)
	(car exprs)
	(with-output-to-string (out)
	  (write-char #\( out)
	  (loop with first = t for expr in exprs do
	       (if first
		   (setf first nil)
		   (write-char #\| out))
	       (princ expr out))
	  (write-char #\) out)))))

(defun driver-help ()
  (write-string "
Usage:
  cl-ledger [options] [command] [arguments]

Commands:
  balance (bal)
  csv
  print (pr)
  register (reg)

Options:
  -b DATE
        Specify the start DATE of all calculations. Transactions before
        that date will be ignored.

  --display EXPR (-d)
        Display lines that satisfy the expression EXPR.

  -e DATE
        Constrain the report so that transactions on or after DATE are
        not considered.

  --file FILE (-f)
        Read journal data from FILE.

  --limit EXPR (-l)
        Limit postings in calculations.

  -n
        Print only the top level accounts.

  -r
        In a register report show the related account. This is the
        other side of the transaction.

  -s
        Report register as a single subtotal.

  -S EXPR
        Sort the register report based on the value expression EXPR.

"))


(defun process-command-line (&rest args)
  ;; Convert the argument list to canonical Lisp form
  (loop for cell on args for arg = (car cell)
     when (and (stringp arg) (> (length arg) 0)) do
     (if (char= #\: (aref arg 0))
	 (rplaca cell (make-symbol arg))
	 (if-let ((number (ignore-errors (parse-integer arg))))
		 (rplaca cell number))))

  (let (pathnames keywords)
    ;; Handle all of the option-like arguments
    (loop while (and args
		     (plusp (length (first args)))
		     (char= #\- (aref (first args) 0))) do
	 (cond
	   ((or (string= "-f" (first args))
		(string= "--file" (first args)))
	    (setf pathnames (append (list (first (rest args))) pathnames))
	    (setf args (rest args)))

	   ((or (string= "-l" (first args))
		(string= "--limit" (first args)))
	    (setf keywords
		  (append (list :limit (first (rest args))) keywords))
	    (setf args (rest args)))

	   ((or (string= "-d" (first args))
		(string= "--display" (first args)))
	    (setf keywords
		  (append (list :display (first (rest args))) keywords))
	    (setf args (rest args)))

	   ((string= "-b" (first args))
	    (setf keywords
		  (append (list :begin (first (rest args))) keywords))
	    (setf args (rest args)))

	   ((string= "-e" (first args))
	    (setf keywords
		  (append (list :end (first (rest args))) keywords))
	    (setf args (rest args)))

	   ((string= "-r" (first args))
	    (setf keywords (append (list :related t) keywords)))

	   ((string= "-n" (first args))
	    (setf keywords (append (list :collapse t) keywords)))

	   ((string= "-s" (first args))
	    (setf keywords (append (list :subtotal t) keywords)))

	   ((string= "-S" (first args))
	    (setf keywords
		  (append (list :sort (first (rest args))) keywords))
	    (setf args (rest args))))
	 (setf args (rest args)))

    (let ((command (car args))
	  account-regexps
	  not-account-regexps
	  payee-regexps
	  not-payee-regexps
	  in-payee-regexps)
      (setf args (cdr args))

      ;; Extract the account and payee regexps
      (loop while (and args (stringp (first args))) do
	   (let ((arg (first args)))
	     (if (string= arg "--")
		 (setf in-payee-regexps t)
		 (if in-payee-regexps
		     (if (char= #\- (aref arg 0))
			 (push (subseq arg 1) not-payee-regexps)
			 (push arg payee-regexps))
		     (if (char= #\- (aref arg 0))
			 (push (subseq arg 1) not-account-regexps)
			 (push arg account-regexps))))
	     (setf args (rest args))))

      (setf account-regexps     (regexp-opt account-regexps)
	    not-account-regexps (regexp-opt not-account-regexps)
	    payee-regexps	(regexp-opt payee-regexps)
	    not-payee-regexps	(regexp-opt not-payee-regexps))

      (setf keywords
	    (append (and account-regexps
			 (list :account account-regexps))
		    (and not-account-regexps
			 (list :not-account not-account-regexps))
		    (and payee-regexps
			 (list :payee payee-regexps))
		    (and not-payee-regexps
			 (list :not-payee not-payee-regexps))
		    keywords))

      (setf args (append pathnames keywords args))

      (unless pathnames
        (driver-help)
        (error "error: no journal file specified."))

      ;; Execute the command
      (cond ((or (string= "reg" command)
		 (string= "register" command))
	     (apply #'ledger:register-report args))

	    ((or (string= "pr" command)
		 (string= "print" command))
	     (apply #'ledger:print-report args))

	    ((or (string= "bal" command)
		 (string= "balance" command))
	     (apply #'ledger:balance-report args))

            ((string= "csv" command)
             (apply #'ledger:csv-report args))

            (t
             (driver-help))))))

(provide 'driver)

;; driver.lisp ends here
