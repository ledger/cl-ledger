;; This file contains the parser for textual ledger files

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(defpackage :ledger-textual
  (:use :common-lisp :ledger :local-time :periods :cambl :cl-ppcre)
  (:export *directive-handlers*))

(in-package :ledger-textual)

(defvar *date-regexp* "[0-9-./]+")

(defvar *spacing-regexp* (format nil "(?:  |~C| ~C)\\s*" #\Tab #\Tab))

(defvar *comment-regexp* (format nil "(?:~A;(.+))" *spacing-regexp*))

(defvar *entry-heading-scanner*
  (cl-ppcre:create-scanner
   (format nil (concatenate 'string
			    "^(?:(~A)(?:=(~A))?)\\s+(?:(\\*|!)\\s*)?"
			    "(?:\\((.+?)\\)\\s+)?(.+?)~A?$")
	   *date-regexp* *date-regexp* *comment-regexp*)))

(defvar *transaction-scanner*
  (cl-ppcre:create-scanner
   (format nil (concatenate 'string
			    "^\\s+(?:(\\*|!)\\s*)?([\\[(])?(.+?)([\\])])?"
			    "(?:~A(?:([^; ~C].*?)(?:(@@?)\\s*(.+?))?))?~A?$")
	   *spacing-regexp* #\Tab *comment-regexp*)))

(defvar *directive-handlers*
  `(((#\; #\* #\% #\#)
     . ,#'(lambda (in journal)
	    (declare (ignore journal))
	    ;; comma begins a comment; gobble up the rest of
	    ;; the line
	    (read-line in nil)))

    ((#\Newline #\Return) . ,#'(lambda (in journal)
				 (declare (ignore journal))
				 (read-char in nil)))

    (#\Y . ,#'(lambda (in journal)
		(read-char in)
		(peek-char t in)
		(setf (journal-default-year journal) (read in)
		      ;; jww (2007-11-15): This is a total hack
		      (journal-date-format journal) "%m/%d")))

    (#\A . ,#'(lambda (in journal)
		(read-char in)
		(peek-char t in)
		(setf (journal-default-account journal)
		      (find-account journal (read-line in)
				    :create-if-not-exists-p t))))

    (#\D . ,#'(lambda (in journal)
		(declare (ignore journal))
		(read-char in)
		(peek-char t in)
		(cambl:read-amount in)))

    (#\N . ,#'(lambda (in journal)
		(declare (ignore journal))
		(read-char in)
		(peek-char t in)
		(let ((commodity
		       (cambl:find-commodity (read-line in)
					     :create-if-not-exists-p t)))
		  (setf (cambl::get-no-market-price-p
			 (cambl::commodity-base commodity)) t))))

    (,#'digit-char-p
     . ,#'(lambda (in journal)
	    (let ((entry (read-plain-entry in journal)))
	      (if entry
		  (add-to-contents journal entry)
		  (error "Failed to read entry at position ~S~%"
			 (file-position in))))))

    ((#\@ #\!)
     . ,#'(lambda (in journal)
	    (declare (ignore journal))
	    (let ((symbol (read in))
		  (argument (read-line in)))
	      (if (symbolp symbol)
		  (progn
		    (setf symbol
			  (find-symbol (format nil "ledger-text-directive/~A"
					       (symbol-name symbol))))
		    (if (and symbol (functionp symbol))
			(funcall (locally
				     #+sbcl(declare (sb-ext:muffle-conditions
						     sb-ext:code-deletion-note))
				     (fdefinition symbol))
				 argument)
			argument))
		  argument))))

    (#\( . ,#'(lambda (in journal)
		(declare (ignore journal))
		(if *allow-embedded-lisp*
		    (eval (read in))
		    (error "Embedded Lisp not allowed, set `LEDGER:*ALLOW-EMBEDDED-LISP*'"))))))

(defun read-transaction (in entry)
  (declare (type stream in))
  (let* ((beg-pos (file-position in))
	 (xact-line (read-line in nil))
	 (groups (and xact-line
		      (nth-value 1 (cl-ppcre:scan-to-strings
				    *transaction-scanner* xact-line)))))
    (when groups
      (let ((status (aref groups 0))
	    (open-bracket (aref groups 1))
	    (account-name (aref groups 2))
	    (close-bracket (aref groups 3))
	    (amount-expr (aref groups 4))
	    (cost-specifier (aref groups 5))
	    (cost-expr (aref groups 6))
	    (note (aref groups 7))
	    amount cost)

	(when amount-expr
	  (with-input-from-string (in amount-expr)
	    (setf amount (cambl:read-amount in))
	    (when (peek-char t in nil)
	      (file-position in 0)
	      (setf amount
		    (make-value-expr :string   amount-expr
				     :function (read-value-expr in))))))
	(when cost-expr
	  (with-input-from-string (in cost-expr)
	    (setf cost (cambl:read-amount* in))
	    (when (peek-char t in nil)
	      (file-position in 0)
	      (setf cost
		    (make-value-expr :string   cost-expr
				     :function (read-value-expr in))))
	    (unless cost
	      (error "Failed to read cost expression: ~S" cost-expr))))

	(let ((virtualp (and open-bracket
			     (if (string= "(" open-bracket)
				 (string= ")" close-bracket)
				 (string= "]" close-bracket)))))
	  (make-transaction
	   :entry entry
	   ;;:actual-date
	   ;;:effective-date
	   :status (cond ((string= status "*")
			  'cleared)
			 ((string= status "!")
			  'pending)
			 (t
			  'uncleared))
	   :account (find-account (entry-journal entry) account-name
				  :create-if-not-exists-p t)
	   :amount amount
	   :cost (if cost
		     (if (string= "@" cost-specifier)
			 (multiply cost amount)
			 cost))
	   :note note
	   ;;:tags
	   :position (make-item-position :begin-char beg-pos
					 :end-char (file-position in))
	   :virtualp virtualp
	   :must-balance-p (if virtualp
			       (string= open-bracket "[")
			       t)))))))

(defun read-plain-entry (in journal)
  "Read in the header line for the entry, which has the syntax:
  
    (DATE(=DATE)?)( (*|!))?( (\((.+?)\)))? (.+)(:spacer:;(.+))?
  
  :spacer: means: two spaces, a tab, or a space and a tab, followed by any
  amount of whitespace.
  
  The groups identified in this regular expression (found in the scanner
  *entry-heading-scanner*) have these meanings:
  
  1 - The actual date of the entry.
  2 - The (optional) effective date of the entry.
  4 - The (optional) status of the entry: *=cleared, !=pending.
  6 - The (optional) \"code\" for the entry; has no meaning to Ledger.
  7 - The payee or description of the entry.
  9 - A comment giving further details about the entry."
  (declare (type stream in))
  (declare (type journal journal))
  (let* ((beg-pos (file-position in))
	 (heading-line (read-line in nil))
	 (groups (and heading-line
		      (nth-value 1 (cl-ppcre:scan-to-strings
				    *entry-heading-scanner* heading-line)))))
    (when groups
      (let ((actual-date (aref groups 0))
	    (effective-date (aref groups 1))
	    (status (aref groups 2))
	    (code (aref groups 3))
	    (payee (aref groups 4))
	    (note (aref groups 5)))
	(let ((entry (make-instance
		      'entry
		      :journal journal
		      :actual-date (parse-journal-date journal actual-date)
		      :effective-date
		      (and effective-date
			   (parse-journal-date journal effective-date))
		      :status (cond ((string= status "*")
				     'cleared)
				    ((string= status "!")
				     'pending)
				    (t
				     'uncleared))
		      :code code
		      :payee payee
		      :note note
		      :position
		      (make-item-position :begin-char beg-pos
					  :end-char (file-position in)))))
	  (loop
	     for transaction = (read-transaction in entry)
	     while transaction do
	     (add-transaction entry transaction))

	  (normalize-entry entry))))))

(defun read-journal (in binder)
  (declare (type stream in))
  (declare (type binder binder))
  (let ((journal (make-instance 'journal :binder binder)))
    (loop
       for c = (peek-char nil in nil)
       while c do
       (let ((handler
	      (cdr
	       (assoc-if
		#'(lambda (key)
		    (cond
		      ((characterp key)
		       (char= c key))
		      ((listp key)
		       (member c key :test #'char=))
		      ((functionp key)
		       (funcall key c))
		      (t
		       (error "Unexpected element in `*directive-handlers*': ~S"
			      key))))
		*directive-handlers*))))
	 (if handler
	     (funcall handler in journal)
	     (progn
	       (format t "Unhandled directive (pos ~D): ~C~%"
		       (file-position in) c)
	       (read-line in nil)))))
    journal))

(defun ledger-text-directive/include (argument)
  (declare (ignorable argument))
  (format t "@include (~A)~%" argument))
(defun ledger-text-directive/account (argument)
  (declare (ignorable argument))
  (format t "@include (~A)~%" argument))
(defun ledger-text-directive/end (argument)
  (declare (ignorable argument))
  (format t "@include (~A)~%" argument))
(defun ledger-text-directive/alias (argument)
  (declare (ignorable argument))
  (format t "@include (~A)~%" argument))
(defun ledger-text-directive/def (argument)
  (declare (ignorable argument))
  (format t "@include (~A)~%" argument))

;; if (word == "include") {
;;   push_var<std::string>	  save_path(path);
;;   push_var<unsigned int>  save_src_idx(src_idx);
;;   push_var<unsigned long> save_beg_pos(beg_pos);
;;   push_var<unsigned long> save_end_pos(end_pos);
;;   push_var<unsigned int>  save_linenum(linenum);
;; 
;;   path = p;
;;   if (path[0] != '/' && path[0] != '\\' && path[0] != '~') {
;;     std::string::size_type pos = save_path.prev.rfind('/');
;;     if (pos == std::string::npos)
;;       pos = save_path.prev.rfind('\\');
;;     if (pos != std::string::npos)
;;       path = std::string(save_path.prev, 0, pos + 1) + path;
;;   }
;;   path = resolve_path(path);
;; 
;;   DEBUG_PRINT("ledger-textual.include", "line " << linenum << ": " <<
;; 	      "Including path '" << path << "'");
;; 
;;   include_stack.push_back(std::pair<std::string, int>
;; 			  (journal->sources.back(), linenum - 1));
;;   count += parse_journal_file(path, config, journal,
;; 			      account_stack.front());
;;   include_stack.pop_back();
;; }
;; else if (word == "account") {
;;   account_t * acct;
;;   acct = account_stack.front()->find_account(p);
;;   account_stack.push_front(acct);
;; }
;; else if (word == "end") {
;;   account_stack.pop_front();
;; }
;; else if (word == "alias") {
;;   char * b = p;
;;   if (char * e = std::strchr(b, '=')) {
;;     char * z = e - 1;
;;     while (std::isspace(*z))
;;       *z-- = '\0';
;;     *e++ = '\0';
;;     e = skip_ws(e);
;; 
;;     // Once we have an alias name (b) and the target account
;;     // name (e), add a reference to the account in the
;;     // `account_aliases' map, which is used by the transaction
;;     // parser to resolve alias references.
;;     account_t * acct = account_stack.front()->find_account(e);
;;     std::pair<accounts_map::iterator, bool> result
;;       = account_aliases.insert(accounts_pair(b, acct));
;;     assert(result.second);
;;   }
;; }
;; else if (word == "def") {
;;   if (! global_scope.get())
;;     init_value_expr();
;;   parse_value_definition(p);
;; }

(pushnew #'read-journal *registered-parsers*)

(provide 'textual)

;; textual.lisp ends here
