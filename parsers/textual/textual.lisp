;; This file contains the parser for textual ledger files

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(defpackage :ledger-textual
  (:use :common-lisp :ledger :local-time :periods :cambl :cl-ppcre)
  (:export *directive-handlers*))

(in-package :ledger-textual)

(defvar *date-regexp* "[0-9-./]+")

(defvar *spacing-regexp* (format nil "(?:  |~C| ~C)\\s*" #\Tab #\Tab))

(defvar *comment-regexp* (format nil "(?:[ ~C]+;(.+))" #\Tab))

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
     . ,#'(lambda (in line journal)
	    (declare (ignore line journal))
	    ;; comma begins a comment; gobble up the rest of
	    ;; the line
	    (read-line in nil)
	    1))

    (#\Return
     . ,#'(lambda (in line journal)
	    (declare (ignore line journal))
	    ;; DOS uses CRLF; Mac uses CR
	    (if (eq #\Newline (peek-char nil in nil))
		(read-char in nil))
	    1))

    (#\Newline
     . ,#'(lambda (in line journal)
	    (declare (ignore line journal))
	    (read-char in nil)
	    1))

    (#\F . ,#'(lambda (in line journal)
                (declare (ignore line))
                (prog1 1
                  (read-char in)
                  (peek-char t in)
                  (setf (journal-date-format journal) (read-line in nil)))))
    (#\Y . ,#'(lambda (in line journal)
		(declare (ignore line))
		(read-char in)
		(peek-char t in)
		(setf (journal-default-year journal)
		      (read-preserving-whitespace in)
		      ;; jww (2007-11-15): This is a total hack
		      (journal-date-format journal) "%m/%d")
		0))

    (#\A . ,#'(lambda (in line journal)
		(declare (ignore line))
		(read-char in)
		(peek-char t in)
		(setf (journal-default-account journal)
		      (find-account journal (read-line in)
				    :create-if-not-exists-p t))
		1))

    (#\D . ,#'(lambda (in line journal)
		(declare (ignore line journal))
		(read-char in)
		(peek-char t in)
		(cambl:read-amount in)
		0))

    (#\N . ,#'(lambda (in line journal)
		(declare (ignore line journal))
		(read-char in)
		(peek-char t in)
		(let ((commodity
		       (find-commodity (read-line in)
				       :create-if-not-exists-p t)))
		  (assert (not (annotated-commodity-p commodity)))
		  ;; jww (2007-12-05): export this
		  (setf (commodity-no-market-price-p commodity) t)
		  1)))

    (,#'digit-char-p
     . ,#'(lambda (in line journal)
	    (multiple-value-bind (entry lines)
		(read-plain-entry in line journal)
	      (if entry
		  (add-to-contents journal entry)
		  (error "Failed to read entry at line ~S~%" line))
	      lines)))

    ((#\@ #\!)
     . ,#'(lambda (in line journal)
	    (declare (ignore journal))
	    (let ((symbol (read-preserving-whitespace in))
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
			(error "Unrecognized directive \"~S~\" at line ~D"
			       argument line)))
		  (error "Unrecognized directive \"~S~\" at line ~D"
			 argument line))
	      1)))

    (#\( . ,#'(lambda (in line journal)
		(declare (ignore journal))
		(if *allow-embedded-lisp*
		    (let ((begin-pos (file-position in)) end-pos lines)
		      (eval (read-preserving-whitespace in))
		      (setf end-pos (file-position in))
		      (file-position in begin-pos)
		      (loop repeat (- end-pos begin-pos) do
			   (let ((ch (read-char in)) found)
			     (loop while (member ch '(#\Newline #\Return)) do
				  (setf found t ch (read-char in)))
			     (if found (incf lines))))
		      lines)
		    (error "Embedded Lisp not allowed at line ~D, ~
                            set LEDGER:*ALLOW-EMBEDDED-LISP*" line))))))

(defun read-transaction (in line entry)
  (declare (type stream in))
  (let* ((xact-line (string-right-trim '(#\Space #\Tab)
				       (read-line in nil)))
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
	   ;; jww (2007-12-09): NYI
	   ;;:actual-date
	   ;;:effective-date
	   :status (cond ((string= status "*") :cleared)
			 ((string= status "!") :pending)
			 (t :uncleared))
	   :account (find-account (entry-journal entry)
				  (string-right-trim '(#\Space #\Tab)
						     account-name)
				  :create-if-not-exists-p t)
	   :amount amount
	   :cost (if cost
		     (if (string= "@" cost-specifier)
			 (multiply cost amount)
			 cost))
	   :note note
	   :position (make-item-position :begin-line line
					 :end-line line)
	   :virtualp virtualp
	   :must-balance-p (if virtualp
			       (string= open-bracket "[")
			       t)))))))

(defun read-plain-entry (in line journal)
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
  (let* ((heading-line (read-line in nil))
	 (groups (and heading-line
		      (nth-value 1 (cl-ppcre:scan-to-strings
				    *entry-heading-scanner* heading-line))))
	 (lines 1))
    (when groups
      (let ((actual-date (aref groups 0))
	    (effective-date (aref groups 1))
	    (status (aref groups 2))
	    (code (aref groups 3))
	    (payee (aref groups 4))
	    (note (aref groups 5)))
	(let ((entry
	       (make-instance
		'entry
		:journal journal
		:actual-date (parse-journal-date journal actual-date)
		:effective-date
		(and effective-date
		     (parse-journal-date journal effective-date))
		:status (cond ((string= status "*") :cleared)
			      ((string= status "!") :pending)
			      (t :uncleared))
		:code code
		:payee (string-right-trim '(#\Space #\Tab) payee)
		:note note
		:position (make-item-position :begin-line line))))

	  (loop for transaction = (read-transaction in (+ line lines) entry)
	     while transaction do (add-transaction entry transaction)
	     (incf lines))
	  (incf lines)

	  (setf (item-position-end-line (entry-position entry))
		(+ line lines))

	  (normalize-entry entry)
	  (values entry lines))))))

(defun read-textual-journal (in binder)
  (declare (type stream in))
  (declare (type binder binder))
  (let ((journal (make-instance 'journal :binder binder))
	(line-number 1))
    (loop for c = (peek-char nil in nil) while c do
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
	       (incf line-number (funcall handler in line-number journal))
	       (progn
		 (format t "Unhandled directive (line ~D): ~C~%" line-number c)
		 (read-line in nil)
		 (incf line-number)))))
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

(pushnew #'read-textual-journal *registered-parsers*)

(provide 'textual)

;; textual.lisp ends here
