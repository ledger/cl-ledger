;;; cl-ledger.el --- Major mode for editing CL-Ledger data files

;; Copyright (C) 2007 John Wiegley (johnw AT newartisans DOT com)

;; Emacs Lisp Archive Entry
;; Filename: cl-ledger.el
;; Version: 4.0
;; Date: Tue 04-Dec-2007
;; Keywords: data
;; Author: John Wiegley (johnw AT newartisans DOT com)
;; Maintainer: John Wiegley (johnw AT newartisans DOT com)
;; Description: Helper code for using my "ledger" command-line tool
;; URL: http://www.newartisans.com/johnw/emacs.html
;; Compatibility: Emacs22

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; To use this module: Load this file, open a ledger data file, and
;; type M-x ledger-mode.  Once this is done, you can type:
;;
;;   C-c C-a       add a new entry, based on previous entries
;;   C-c C-e       toggle cleared status of an entry
;;   C-c C-y       set default year for entry mode
;;   C-c C-m       set default month for entry mode
;;   C-c C-r       reconcile uncleared entries related to an account
;;   C-c C-o C-r   run a ledger report
;;   C-C C-o C-g   goto the ledger report buffer
;;   C-c C-o C-e   edit the defined ledger reports
;;   C-c C-o C-s   save a report definition based on the current report
;;   C-c C-o C-a   rerun a ledger report
;;   C-c C-o C-k   kill the ledger report buffer
;;
;; In the reconcile buffer, use SPACE to toggle the cleared status of
;; a transaction, C-x C-s to save changes (to the ledger file as
;; well).
;;
;; The ledger reports command asks the user to select a report to run
;; then creates a report buffer containing the results of running the
;; associated command line.  Its' behavior is modified by a prefix
;; argument which, when given, causes the generated command line that
;; will be used to create the report to be presented for editing
;; before the report is actually run.  Arbitrary unnamed command lines
;; can be run by specifying an empty name for the report.  The command
;; line used can later be named and saved for future use as a named
;; report from the generated reports buffer.
;;
;; In a report buffer, the following keys are available:
;;   (space)  scroll up
;;   e        edit the defined ledger reports
;;   s        save a report definition based on the current report
;;   q        quit the report (return to ledger buffer)
;;   r        redo the report
;;   k        kill the report buffer

(require 'cl)
(require 'pcomplete)
(require 'esh-util)
(require 'esh-arg)

(defconst cl-ledger-version "4.0"
  "The version of cl-ledger.el currently loaded")

(defgroup cl-ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defcustom cl-ledger-file "~/src/ledger/doc/sample.dat"
  "Path to the default ledger data file."
  :type 'file
  :group 'cl-ledger)

(defcustom cl-ledger-clear-whole-entries nil
  "If non-nil, clear whole entries, not individual transactions."
  :type 'boolean
  :group 'cl-ledger)

;;;_* CL-Ledger major-mode

(defconst +cl-ledger-entry-regexp+
  (concat "^[0-9/.=-]+\\(\\s-+[*!]\\)?\\(\\s-+(.*?)\\)?\\s-+"
	  "\\(.+?\\)\\(\\s-+;\\(.+\\)\\)?$"))

(defvar bold 'bold)
(defvar cl-ledger-font-lock-keywords
  `((,+cl-ledger-entry-regexp+ 3 bold)
    (";.+" . font-lock-comment-face)
    ("^\\s-+.+?\\(  \\|\t\\|\n\\|\\s-+$\\)" . font-lock-keyword-face))
  "Default expressions to highlight in Ledger mode.")

(defvar cl-ledger-mode-abbrev-table)

;;;###autoload
(define-derived-mode cl-ledger-mode text-mode "CL-Ledger"
  "A mode for editing ledger data files."
  (set (make-local-variable 'comment-start) " ; ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-tabs-mode) nil)

  (if (boundp 'font-lock-defaults)
      (set (make-local-variable 'font-lock-defaults)
	   '(cl-ledger-font-lock-keywords nil t)))

  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'cl-ledger-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'cl-ledger-complete-at-point)
  (set (make-local-variable 'pcomplete-termination-string) "")

  (add-hook 'after-save-hook #'cl-ledger-clear-cache-variables nil t)

  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?a)] 'cl-ledger-add-entry)
    (define-key map [(control ?c) (control ?d)] 'cl-ledger-delete-current-entry)
    (define-key map [(control ?c) (control ?y)] 'cl-ledger-set-year)
    (define-key map [(control ?c) (control ?m)] 'cl-ledger-set-month)
    (define-key map [(control ?c) (control ?c)] 'cl-ledger-toggle-current)
    (define-key map [(control ?c) (control ?e)] 'cl-ledger-toggle-current-entry)
    (define-key map [(control ?c) (control ?r)] 'cl-ledger-reconcile)
    (define-key map [(control ?c) (control ?s)] 'cl-ledger-sort)

    (define-key map [tab] 'pcomplete)
    (define-key map [(control ?i)] 'pcomplete)
    (define-key map [(control ?c) tab] 'cl-ledger-fully-complete-entry)
    (define-key map [(control ?c) (control ?i)] 'cl-ledger-fully-complete-entry)

    (define-key map [(control ?c) (control ?o) (control ?r)] 'cl-ledger-report)
    (define-key map [(control ?c) (control ?o) (control ?g)] 'cl-ledger-report-goto)
    (define-key map [(control ?c) (control ?o) (control ?a)] 'cl-ledger-report-redo)
    (define-key map [(control ?c) (control ?o) (control ?s)] 'cl-ledger-report-save)
    (define-key map [(control ?c) (control ?o) (control ?e)] 'cl-ledger-report-edit)
    (define-key map [(control ?c) (control ?o) (control ?k)] 'cl-ledger-report-kill)))

;;;_* Interaction code (via SLIME)

(defvar *cl-ledger-entries* nil)
(make-variable-buffer-local '*cl-ledger-entries*)

(defvar *cl-ledger-unique-payees* nil)
(make-variable-buffer-local '*cl-ledger-unique-payees*)

(defvar *cl-ledger-unique-accounts* nil)
(make-variable-buffer-local '*cl-ledger-unique-accounts*)

(defvar *cl-ledger-account-tree* nil)
(make-variable-buffer-local '*cl-ledger-account-tree*)

(defun cl-ledger-clear-cache-variables ()
  (setf *cl-ledger-entries* nil
	*cl-ledger-unique-payees* nil
	*cl-ledger-unique-accounts* nil
	*cl-ledger-account-tree* nil))

(defun cl-ledger-entries ()
  (or *cl-ledger-entries*
      (prog1
	  (setf *cl-ledger-entries*
		(slime-eval `(ledger:sexp-report ,(buffer-file-name
						   (current-buffer)))))
	(save-excursion
	  (dolist (entry *cl-ledger-entries*)
	    (goto-line (nth 0 entry))
	    (add-text-properties (point) (line-end-position)
				 (list 'cl-ledger-what 'entry))
	    (let ((begin (point)))
	      (dolist (xact (nth 4 entry))
		(forward-line 1)
		(assert (= (line-number-at-pos) (nth 0 xact)))
		(add-text-properties (line-beginning-position)
				     (1+ (line-end-position))
				     (list 'cl-ledger-what 'transaction
					   'cl-ledger-xact xact)))
	      (forward-line 1)
	      (add-text-properties begin (point)
				   (list 'cl-ledger-entry entry))))))))

(defun cl-ledger-unique-payees ()
  (or *cl-ledger-unique-payees*
      (setf *cl-ledger-unique-payees*
	    (pcomplete-uniqify-list
	     (mapcar #'(lambda (entry)
			 (nth 3 entry))
		     (cl-ledger-entries))))))

(defun cl-ledger-unique-accounts ()
  (or *cl-ledger-unique-accounts*
      (setf *cl-ledger-unique-accounts*
	    (pcomplete-uniqify-list
	     (let (result)
	       (mapcar #'(lambda (entry)
			   (dolist (xact (nth 4 entry))
			     (push (nth 2 xact) result)))
		       (cl-ledger-entries))
	       (nreverse result))))))

;;;_* Context-sensitive completion

(defsubst cl-ledger-thing-at-point ()
  (let ((type (get-text-property (point) 'cl-ledger-what)))
    (or type
	(progn
	  (cl-ledger-entries)
	  (cl-ledger-thing-at-point)))))

(defun cl-ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let* ((info (cons (cl-ledger-thing-at-point) (point)))
	 (begin (cdr info))
	 (end (point))
	 begins args)
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
	(skip-chars-forward " \t\n")
	(setq begins (cons (point) begins))
	(setq args (cons (buffer-substring-no-properties
			  (car begins) end)
			 args)))
      (cons (reverse args) (reverse begins)))))

(defun cl-ledger-account-tree ()
  (unless *cl-ledger-account-tree*
    (setf *cl-ledger-account-tree* (list t))
    (dolist (account-path (cl-ledger-unique-accounts))
      (let ((root *cl-ledger-account-tree*))
	(dolist (element (split-string account-path ":"))
	  (let ((entry (assoc element root)))
	    (if entry
		(setq root (cdr entry))
	      (setq entry (cons element (list t)))
	      (nconc root (list entry))
	      (setq root (cdr entry))))))))
  *cl-ledger-account-tree*)

(defun cl-ledger-accounts ()
  (let* ((current (caar (cl-ledger-parse-arguments)))
	 (elements (and current (split-string current ":")))
	 (root (cl-ledger-account-tree))
	 (prefix nil))

    (while (cdr elements)
      (let ((entry (assoc (car elements) root)))
	(if entry
	    (setq prefix (concat prefix (and prefix ":")
				 (car elements))
		  root (cdr entry))
	  (setq root nil elements nil)))
      (setq elements (cdr elements)))

    (and root
	 (sort
	  (mapcar (function
		   (lambda (x)
		     (let ((term (if prefix
				     (concat prefix ":" (car x))
				   (car x))))
		       (if (> (length (cdr x)) 1)
			   (concat term ":")
			 term))))
		  (cdr root))
	  'string-lessp))))

(defun cl-ledger-complete-at-point ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (while (pcomplete-here
	  (if (eq (cl-ledger-thing-at-point) 'entry)
	      (cl-ledger-unique-payees)
	    (cl-ledger-accounts)))))

(defun cl-ledger-fully-complete-entry ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (let ((name (caar (cl-ledger-parse-arguments)))
	xacts)
    (save-excursion
      (when (eq 'entry (cl-ledger-thing-at-point))
	(when (re-search-backward
	       (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
		       (regexp-quote name) "\\(\t\\|\n\\| [ \t]\\)") nil t)
	  (forward-line)
	  (while (looking-at "^\\s-+")
	    (setq xacts (cons (buffer-substring-no-properties
			       (line-beginning-position)
			       (line-end-position))
			      xacts))
	    (forward-line))
	  (setq xacts (nreverse xacts)))))
    (when xacts
      (save-excursion
	(insert ?\n)
	(while xacts
	  (insert (car xacts) ?\n)
	  (setq xacts (cdr xacts))))
      (forward-line)
      (goto-char (line-end-position))
      (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
	  (goto-char (match-end 0))))))

;;;_* Toggling entry and transaction state

(defun cl-ledger-toggle-current-entry (&optional style)
  (interactive)
  (let (clear)
    (save-excursion
      (when (or (looking-at "^[0-9]")
		(re-search-backward "^[0-9]" nil t))
	(skip-chars-forward "0-9./=")
	(delete-horizontal-space)
	(if (member (char-after) '(?\* ?\!))
	    (progn
	      (delete-char 1)
	      (if (and style (eq style :cleared))
		  (insert " *")))
	  (if (and style (eq style :pending))
	      (insert " ! ")
	    (insert " * "))
	  (setq clear t))))
    clear))

(defun cl-ledger-move-to-next-field ()
  (re-search-forward "\\(  \\|\t\\)" (line-end-position) t))

(defun cl-ledger-toggle-state (state &optional style)
  (if (not (null state))
      (if (and style (eq style :cleared))
	  :cleared)
    (if (and style (eq style :pending))
	:pending
      :cleared)))

(defun cl-ledger-entry-state ()
  (save-excursion
    (when (or (looking-at "^[0-9]")
	      (re-search-backward "^[0-9]" nil t))
      (skip-chars-forward "0-9./=")
      (skip-syntax-forward " ")
      (cond ((looking-at "!\\s-*") :pending)
	    ((looking-at "\\*\\s-*") :cleared)
	    (t :uncleared)))))

(defun cl-ledger-transaction-state ()
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-syntax-forward " ")
    (cond ((looking-at "!\\s-*") :pending)
	  ((looking-at "\\*\\s-*") :cleared)
	  (t (cl-ledger-entry-state)))))

(defun cl-ledger-toggle-current-transaction (&optional style)
  "Toggle the cleared status of the transaction under point.
Optional argument STYLE may be `pending' or `cleared', depending
on which type of status the caller wishes to indicate (default is
`cleared').
This function is rather complicated because it must preserve both
the overall formatting of the ledger entry, as well as ensuring
that the most minimal display format is used.  This could be
achieved more certainly by passing the entry to ledger for
formatting, but doing so causes inline math expressions to be
dropped."
  (interactive)
  (let ((bounds (cl-ledger-current-entry-bounds))
	clear cleared)
    ;; Uncompact the entry, to make it easier to toggle the
    ;; transaction
    (save-excursion
      (goto-char (car bounds))
      (skip-chars-forward "0-9./= \t")
      (setq cleared (and (member (char-after) '(?\* ?\!))
			 (char-after)))
      (when cleared
	(let ((here (point)))
	  (skip-chars-forward "*! ")
	  (let ((width (- (point) here)))
	    (when (> width 0)
	      (delete-region here (point))
	      (if (cl-ledger-move-to-next-field)
		  (insert (make-string width ? ))))))
	(forward-line)
	(while (looking-at "[ \t]")
	  (skip-chars-forward " \t")
	  (insert cleared " ")
	  (when (cl-ledger-move-to-next-field)
	    (goto-char (match-beginning 0))
	    (delete-char 2))
	  (forward-line))))
    ;; Toggle the individual transaction
    (save-excursion
      (goto-char (line-beginning-position))
      (when (looking-at "[ \t]")
	(skip-chars-forward " \t")
	(let ((here (point))
	      (cleared (member (char-after) '(?\* ?\!))))
	  (skip-chars-forward "*! ")
	  (let ((width (- (point) here)))
	    (when (> width 0)
	      (delete-region here (point))
	      (save-excursion
		(if (cl-ledger-move-to-next-field)
		    (insert (make-string width ? ))))))
	  (let (inserted)
	    (if cleared
		(if (and style (eq style :cleared))
		    (progn
		      (insert "* ")
		      (setq inserted t)))
	      (if (and style (eq style :pending))
		  (progn
		    (insert "! ")
		    (setq inserted t))
		(progn
		  (insert "* ")
		  (setq inserted t))))
	    (when (and inserted
		       (cl-ledger-move-to-next-field))
	      (goto-char (match-beginning 0))
	      (delete-char 2))
	    (setq clear inserted)))))
    ;; Clean up the entry so that it displays minimally
    (save-excursion
      (goto-char (car bounds))
      (forward-line)
      (let ((first t)
	    (state ? )
	    (hetero nil))
	(while (and (not hetero) (looking-at "[ \t]"))
	  (skip-chars-forward " \t")
	  (let ((cleared (if (member (char-after) '(?\* ?\!))
			     (char-after)
			   ? )))
	    (if first
		(setq state cleared
		      first nil)
	      (if (/= state cleared)
		  (setq hetero t))))
	  (forward-line))
	(when (and (not hetero) (/= state ? ))
	  (goto-char (car bounds))
	  (forward-line)
	  (while (looking-at "[ \t]")
	    (skip-chars-forward " \t")
	    (let ((here (point)))
	      (skip-chars-forward "*! ")
	      (let ((width (- (point) here)))
		(when (> width 0)
		  (delete-region here (point))
		  (if (cl-ledger-move-to-next-field)
		      (insert (make-string width ? ))))))
	    (forward-line))
	  (goto-char (car bounds))
	  (skip-chars-forward "0-9./= \t")
	  (insert state " ")
	  (when (cl-ledger-move-to-next-field)
	    (goto-char (match-beginning 0))
	    (delete-char 2)))))

    (goto-char (car bounds))
    (cl-ledger-set-entity
     (nth 0 (get-text-property (point) 'cl-ledger-entry))
     'ledger:entry-status (cl-ledger-entry-state))
    (forward-line 1)
    (while (< (point) (cdr bounds))
      (cl-ledger-set-entity
       (nth 0 (get-text-property (point) 'cl-ledger-xact))
       'ledger:xact-status (cl-ledger-transaction-state))
      (forward-line 1))

    clear))

(defun cl-ledger-set-entity (line function value)
  (slime-eval
   `(cl:setf (,function (ledger:find-current-entity
			 ,(buffer-file-name (current-buffer)) ,line))
	     ,value)))

(defun cl-ledger-toggle-current (&optional style)
  (interactive)
  (cl-ledger-entries)
  (if (or cl-ledger-clear-whole-entries
	  (eq 'entry (cl-ledger-thing-at-point)))
      (cl-ledger-toggle-current-entry
       (or style (if current-prefix-arg :pending)))
    (cl-ledger-toggle-current-transaction
     (or style (if current-prefix-arg :pending)))))

;;;_* Account reconciling

(defvar cl-ledger-buf nil)
(defvar cl-ledger-acct nil)
(defvar cl-ledger-goal nil)

(defun cl-ledger-reconcile-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
	(line (count-lines (point-min) (point))))
    (erase-buffer)
    (cl-ledger-do-reconcile)
    (set-buffer-modified-p t)
    (goto-char (point-min))
    (forward-line line)))

(defun cl-ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where))
	(account cl-ledger-acct)
	(inhibit-read-only t)
	cleared)
    (with-current-buffer cl-ledger-buf
      (goto-char (cdr where))
      (setq cleared (cl-ledger-toggle-current :pending)))
    (if cleared
	(add-text-properties (line-beginning-position)
			     (line-end-position)
			     (list 'face 'bold))
      (remove-text-properties (line-beginning-position)
			      (line-end-position)
			      (list 'face)))
    (forward-line)
    (cl-ledger-reconcile-update-mode-string)))

(defun cl-ledger-reconcile-refresh-after-save ()
  (let ((buf (get-buffer "*Reconcile*")))
    (if buf
	(with-current-buffer buf
	  (cl-ledger-reconcile-refresh)
	  (set-buffer-modified-p nil)))))

(defun cl-ledger-reconcile-add ()
  (interactive)
  (with-current-buffer cl-ledger-buf
    (call-interactively #'cl-ledger-add-entry))
  (cl-ledger-reconcile-refresh))

(defun cl-ledger-reconcile-delete ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (with-current-buffer cl-ledger-buf
      (goto-char (cdr where))
      (cl-ledger-delete-current-entry))
    (let ((inhibit-read-only t))
      (goto-char (line-beginning-position))
      (delete-region (point) (1+ (line-end-position)))
      (set-buffer-modified-p t))))

(defun cl-ledger-reconcile-visit ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (switch-to-buffer-other-window cl-ledger-buf)
    (goto-char (cdr where))))

(defun cl-ledger-reconcile-save ()
  (interactive)
  (with-current-buffer cl-ledger-buf
    (save-buffer))
  (set-buffer-modified-p nil))

(defun cl-ledger-reconcile-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun cl-ledger-reconcile-finish ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((where (get-text-property (point) 'where))
	    (face  (get-text-property (point) 'face)))
	(if (eq face 'bold)
	    (with-current-buffer cl-ledger-buf
	      (goto-char (car where))
	      (unless (eq (cl-ledger-transaction-state) :cleared)
		(cl-ledger-toggle-current :cleared)))))
      (forward-line 1)))
  (cl-ledger-reconcile-save))

(defun cl-ledger-do-reconcile ()
  (let ((items
	 (slime-eval
	  `(ledger:sexp-report ,(buffer-file-name cl-ledger-buf)
			       :account ,cl-ledger-acct
			       :display "!X"))))
    (dolist (item items)
      (let ((index 1))
	(dolist (xact (nth 4 item))
	  (let ((beg (point))
		(where
		 (with-current-buffer cl-ledger-buf
		   (cons
		    (save-excursion
		      (goto-line (nth 0 item))
		      (point-marker))
		    (if cl-ledger-clear-whole-entries
			(save-excursion
			  (goto-line (nth 0 item))
			  (forward-line (length (nth 4 item)))
			  (point-marker))
		      (save-excursion
			(goto-line (nth 0 xact))
			(point-marker)))))))
	    (insert (format "%s %-30s %-25s %12s %12s\n"
			    (format-time-string "%m/%d" (nth 1 item))
			    (nth 3 item) (nth 2 xact)
			    (nth 3 xact) (nth 4 xact)))
	    (set-text-properties beg (1- (point)) (list 'where where))
	    (unless (eq (nth 1 xact) :uncleared)
	      (add-text-properties beg (1- (point))
				   (list 'face 'bold where))))
	  (setq index (1+ index)))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only t)))

(defun cl-ledger-reconcile (account goal-balance &optional arg)
  (interactive "sAccount to reconcile: \nsTarget balance: \nP")
  (let ((buf (current-buffer))
	(rbuf (get-buffer "*Reconcile*")))
    (if rbuf
	(kill-buffer rbuf))
    (add-hook 'after-save-hook 'cl-ledger-reconcile-refresh-after-save nil t)
    (with-current-buffer
	(pop-to-buffer (get-buffer-create "*Reconcile*"))
      (cl-ledger-reconcile-mode)
      (set (make-local-variable 'cl-ledger-buf) buf)
      (set (make-local-variable 'cl-ledger-acct) account)
      (set (make-local-variable 'cl-ledger-goal) goal-balance)
      (cl-ledger-do-reconcile)
      (cl-ledger-reconcile-update-mode-string))))

(defun cl-ledger-reconcile-update-mode-string ()
  (let ((cleared-total
	 (nth 4 (car (nth 4 (car (slime-eval
				  `(ledger:sexp-report
				    ,(buffer-file-name cl-ledger-buf)
				    :account ,cl-ledger-acct
				    :not-status :uncleared :tail 1))))))))
    ;; jww (2007-12-05): What about when the cleared-total is a balance?
    (setf mode-name
	  (if cleared-total
	      (let ((difference
		     (slime-eval
		      `(cambl:format-value
			(cambl:subtract (cambl:amount* ,cl-ledger-goal)
					(cambl:amount* ,cleared-total))))))
		(format "Reconcile:%s/%s" cleared-total difference))
	    "Reconcile"))))

(defvar cl-ledger-reconcile-mode-abbrev-table)

(define-derived-mode cl-ledger-reconcile-mode text-mode "Reconcile"
  "A mode for reconciling ledger entries."
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'cl-ledger-reconcile-visit)
    (define-key map [return] 'cl-ledger-reconcile-visit)
    (define-key map [(control ?c) (control ?c)] 'cl-ledger-reconcile-finish)
    (define-key map [(control ?x) (control ?s)] 'cl-ledger-reconcile-save)
    (define-key map [(control ?l)] 'cl-ledger-reconcile-refresh)
    (define-key map [? ] 'cl-ledger-reconcile-toggle)
    (define-key map [space] 'cl-ledger-reconcile-toggle)
    (define-key map [?a] 'cl-ledger-reconcile-add)
    (define-key map [?d] 'cl-ledger-reconcile-delete)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line)
    (define-key map [?s] 'cl-ledger-reconcile-save)
    (define-key map [?q] 'cl-ledger-reconcile-quit)
    (use-local-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst cl-ledger-current-year ()
  (format-time-string "%Y"))
(defsubst cl-ledger-current-month ()
  (format-time-string "%m"))

(defvar cl-ledger-year (cl-ledger-current-year)
  "Start a ledger session with the current year, but make it
customizable to ease retro-entry.")
(defvar cl-ledger-month (cl-ledger-current-month)
  "Start a ledger session with the current month, but make it
customizable to ease retro-entry.")

(defun cl-ledger-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun cl-ledger-time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun cl-ledger-find-slot (moment)
  (catch 'found
    (cl-ledger-iterate-entries
     #'(lambda (start date mark desc)
	 (if (ledger-time-less-p moment date)
	     (throw 'found start))))))

(defun cl-ledger-current-entry-bounds ()
  (save-excursion
    (when (or (looking-at "^[0-9]")
	      (re-search-backward "^[0-9]" nil t))
      (let ((beg (point)))
	(while (not (eolp))
	  (forward-line))
	(cons (copy-marker beg) (point-marker))))))

(defun cl-ledger-delete-current-entry ()
  (interactive)
  (let ((bounds (cl-ledger-current-entry-bounds)))
    (delete-region (car bounds) (cdr bounds))))

;; These functions are specific to CL-Ledger

(defun cl-ledger-iterate-entries (callback)
  (dolist (entry (slime-eval
		  `(ledger:register-sexp
		    ,(buffer-file-name (current-buffer)))))
    (apply callback (butlast entry (- (length entry) 4)))))

;; A command-line interface to CL-Ledger, in the style of 2.6

(defun cl-ledger-eval (command &rest args)
  (slime-eval
   `(cl:with-output-to-string
     (str)
     (,command ,(expand-file-name cl-ledger-file)
	       ,@args
	       :output-stream str))))

(defun eshell/ledger (&rest args)
  ;; Convert the argument list to canonical Lisp form
  (let ((cell args))
    (while cell
      (let ((arg (car cell)))
	(if (and (stringp arg)
		 (> (length arg) 0))
	    (if (char-equal ?: (aref arg 0))
		(setcar cell (make-symbol arg))
	      (if (not (text-property-not-all
			0 (length arg) 'number t arg))
		  (setcar cell (string-to-number arg))))))
      (setq cell (cdr cell))))

  (let ((command (car args))
	account-regexps
	payee-regexps
	in-payee-regexps)
    (setq args (cdr args))

    ;; Extract the account and payee regexps
    (dolist (arg args)
      (if (string= arg "--")
	  (setq in-payee-regexps t)
	(if in-payee-regexps
	    (push arg payee-regexps)
	  (push arg account-regexps))))
    (setq account-regexps (regexp-opt account-regexps)
	  payee-regexps (regexp-opt payee-regexps))

    ;; Execute the command
    (cond ((or (string= "reg" command)
	       (string= "register" command))
	   (cl-ledger-eval 'ledger:register-report
			   :account account-regexps
			   :payee payee-regexps))

	  ((or (string= "pr" command)
	       (string= "print" command))
	   (cl-ledger-eval 'ledger:print-report
			   :account account-regexps
			   :payee payee-regexps))

	  ((or (string= "bal" command)
	       (string= "balance" command))
	   (cl-ledger-eval 'ledger:balance-report
			   :account account-regexps
			   :payee payee-regexps)))))

;;; Code relating to Ledger 2.x:

(defcustom cl-ledger-binary-path (executable-find "ledger")
  "Path to the ledger executable."
  :type 'file
  :group 'ledger)

(defcustom cl-ledger-reports
  '(("bal" "ledger -f %(cl-ledger-file) bal")
    ("reg" "ledger -f %(cl-ledger-file) reg")
    ("payee" "ledger -f %(cl-ledger-file) reg -- %(payee)")
    ("account" "ledger -f %(cl-ledger-file) reg %(account)"))
  "Definition of reports to run.

Each element has the form (NAME CMDLINE).  The command line can
contain format specifiers that are replaced with context sensitive
information.  Format specifiers have the format '%(<name>)' where
<name> is an identifier for the information to be replaced.  The
`cl-ledger-report-format-specifiers' alist variable contains a mapping
from format specifier identifier to a lisp function that implements
the substitution.  See the documentation of the individual functions
in that variable for more information on the behavior of each
specifier."
  :type '(repeat (list (string :tag "Report Name")
		       (string :tag "Command Line")))
  :group 'ledger)

(defcustom cl-ledger-report-format-specifiers
  '(("cl-ledger-file" . cl-ledger-report-cl-ledger-file-format-specifier)
    ("payee" . cl-ledger-report-payee-format-specifier)
    ("account" . cl-ledger-report-account-format-specifier))
  "Alist mapping ledger report format specifiers to implementing functions

The function is called with no parameters and expected to return the
text that should replace the format specifier."
  :type 'alist
  :group 'ledger)

(defcustom cl-ledger-default-acct-transaction-indent "    "
  "Default indentation for account transactions in an entry."
  :type 'string
  :group 'ledger)

(defun cl-ledger-add-entry (entry-text)
  (interactive
   (list
    (read-string "Entry: " (concat cl-ledger-year "/" cl-ledger-month "/"))))
  (let* ((args (with-temp-buffer
		 (insert entry-text)
		 (eshell-parse-arguments (point-min) (point-max))))
	 (date (car args))
	 (insert-year t)
	 (cl-ledger-buf (current-buffer))
	 exit-code)
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" date)
	(setq date
	      (encode-time 0 0 0 (string-to-number (match-string 3 date))
			   (string-to-number (match-string 2 date))
			   (string-to-number (match-string 1 date)))))
    (cl-ledger-find-slot date)
    (save-excursion
      (if (re-search-backward "^Y " nil t)
	  (setq insert-year nil)))
    (save-excursion
      (insert
       (with-temp-buffer
	 (setq exit-code
	       (apply #'cl-ledger-run-ledger cl-ledger-buf "entry"
		      (mapcar 'eval args)))
	 (if (= 0 exit-code)
	     (if insert-year
		 (buffer-substring 2 (point-max))
	       (buffer-substring 7 (point-max)))
	   (concat (if insert-year entry-text
		     (substring entry-text 6)) "\n"))) "\n"))))

;; Context sensitivity

(defconst cl-ledger-line-config
  '((entry
     (("^\\(\\([0-9][0-9][0-9][0-9]/\\)?[01]?[0-9]/[0123]?[0-9]\\)[ \t]+\\(\\([!*]\\)[ \t]\\)?[ \t]*\\((\\(.*\\))\\)?[ \t]*\\(.*?\\)[ \t]*;\\(.*\\)[ \t]*$"
       (date nil status nil nil code payee comment))
      ("^\\(\\([0-9][0-9][0-9][0-9]/\\)?[01]?[0-9]/[0123]?[0-9]\\)[ \t]+\\(\\([!*]\\)[ \t]\\)?[ \t]*\\((\\(.*\\))\\)?[ \t]*\\(.*\\)[ \t]*$"
       (date nil status nil nil code payee))))
    (acct-transaction
     (("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\([$]\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account commodity amount nil comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\([$]\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)[ \t]*$"
       (indent account commodity amount nil))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?[0-9]+\\(\\.[0-9]*\\)?\\)[ \t]+\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?[0-9]+\\(\\.[0-9]*\\)?\\)[ \t]+\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?\\(\\.[0-9]*\\)\\)[ \t]+\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?\\(\\.[0-9]*\\)\\)[ \t]+\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]*$"
       (indent account))))))

(defun cl-ledger-extract-context-info (line-type pos)
  "Get context info for current line.

Assumes point is at beginning of line, and the pos argument specifies
where the \"users\" point was."
  (let ((linfo (assoc line-type cl-ledger-line-config))
	found field fields)
    (dolist (re-info (nth 1 linfo))
      (let ((re (nth 0 re-info))
	    (names (nth 1 re-info)))
	(unless found
	  (when (looking-at re)
	    (setq found t)
	    (dotimes (i (length names))
	      (when (nth i names)
		(setq fields (append fields
				     (list
				      (list (nth i names)
					    (match-string-no-properties (1+ i))
					    (match-beginning (1+ i))))))))
	    (dolist (f fields)
	      (and (nth 1 f)
		   (>= pos (nth 2 f))
		   (setq field (nth 0 f))))))))
    (list line-type field fields)))

(defun cl-ledger-context-at-point ()
  "Return a list describing the context around point.

The contents of the list are the line type, the name of the field
point containing point, and for selected line types, the content of
the fields in the line in a association list."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (let ((first-char (char-after)))
	(cond ((equal (point) (line-end-position))
	       '(empty-line nil nil))
	      ((memq first-char '(?\ ?\t))
	       (cl-ledger-extract-context-info 'acct-transaction pos))
	      ((memq first-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	       (cl-ledger-extract-context-info 'entry pos))
	      ((equal first-char ?\=)
	       '(automated-entry nil nil))
	      ((equal first-char ?\~)
	       '(period-entry nil nil))
	      ((equal first-char ?\!)
	       '(command-directive))
	      ((equal first-char ?\;)
	       '(comment nil nil))
	      ((equal first-char ?Y)
	       '(default-year nil nil))
	      ((equal first-char ?P)
	       '(commodity-price nil nil))
	      ((equal first-char ?N)
	       '(price-ignored-commodity nil nil))
	      ((equal first-char ?D)
	       '(default-commodity nil nil))
	      ((equal first-char ?C)
	       '(commodity-conversion nil nil))
	      ((equal first-char ?i)
	       '(timeclock-i nil nil))
	      ((equal first-char ?o)
	       '(timeclock-o nil nil))
	      ((equal first-char ?b)
	       '(timeclock-b nil nil))
	      ((equal first-char ?h)
	       '(timeclock-h  nil nil))
	      (t
	       '(unknown nil nil)))))))

(defun cl-ledger-context-other-line (offset)
  "Return a list describing context of line offset for existing position.

Offset can be positive or negative.  If run out of buffer before reaching
specified line, returns nil."
  (save-excursion
    (let ((left (forward-line offset)))
      (if (not (equal left 0))
	  nil
	(cl-ledger-context-at-point)))))

(defun cl-ledger-context-line-type (context-info)
  (nth 0 context-info))

(defun cl-ledger-context-current-field (context-info)
  (nth 1 context-info))

(defun cl-ledger-context-field-info (context-info field-name)
  (assoc field-name (nth 2 context-info)))

(defun cl-ledger-context-field-present-p (context-info field-name)
  (not (null (cl-ledger-context-field-info context-info field-name))))

(defun cl-ledger-context-field-value (context-info field-name)
  (nth 1 (cl-ledger-context-field-info context-info field-name)))

(defun cl-ledger-context-field-position (context-info field-name)
  (nth 2 (cl-ledger-context-field-info context-info field-name)))

(defun cl-ledger-context-field-end-position (context-info field-name)
  (+ (cl-ledger-context-field-position context-info field-name)
     (length (cl-ledger-context-field-value context-info field-name))))

(defun cl-ledger-context-goto-field-start (context-info field-name)
  (goto-char (cl-ledger-context-field-position context-info field-name)))

(defun cl-ledger-context-goto-field-end (context-info field-name)
  (goto-char (cl-ledger-context-field-end-position context-info field-name)))

(defun cl-ledger-entry-payee ()
  "Returns the payee of the entry containing point or nil."
  (let ((i 0))
    (while (eq (cl-ledger-context-line-type (cl-ledger-context-other-line i))
	       'acct-transaction)
      (setq i (- i 1)))
    (let ((context-info (cl-ledger-context-other-line i)))
      (if (eq (cl-ledger-context-line-type context-info) 'entry)
	  (cl-ledger-context-field-value context-info 'payee)
	nil))))

;; Ledger report mode

(defvar cl-ledger-report-buffer-name "*Ledger Report*")

(defvar cl-ledger-report-name nil)
(defvar cl-ledger-report-cmd nil)
(defvar cl-ledger-report-name-prompt-history nil)
(defvar cl-ledger-report-cmd-prompt-history nil)
(defvar cl-ledger-original-window-cfg nil)

(defvar cl-ledger-report-mode-abbrev-table)

(define-derived-mode cl-ledger-report-mode text-mode "Cl-Ledger-Report"
  "A mode for viewing ledger reports."
  (let ((map (make-sparse-keymap)))
    (define-key map [? ] 'scroll-up)
    (define-key map [?r] 'cl-ledger-report-redo)
    (define-key map [?s] 'cl-ledger-report-save)
    (define-key map [?k] 'cl-ledger-report-kill)
    (define-key map [?e] 'cl-ledger-report-edit)
    (define-key map [?q] 'cl-ledger-report-quit)
    (define-key map [(control ?c) (control ?l) (control ?r)]
      'cl-ledger-report-redo)
    (define-key map [(control ?c) (control ?l) (control ?S)]
      'cl-ledger-report-save)
    (define-key map [(control ?c) (control ?l) (control ?k)]
      'cl-ledger-report-kill)
    (define-key map [(control ?c) (control ?l) (control ?e)]
      'cl-ledger-report-edit)
    (use-local-map map)))

(defun cl-ledger-report-read-name ()
  "Read the name of a ledger report to use, with completion.

The empty string and unknown names are allowed."
  (completing-read "Report name: "
		   cl-ledger-reports nil nil nil
		   'cl-ledger-report-name-prompt-history nil))

(defun cl-ledger-report (report-name edit)
  "Run a user-specified report from `cl-ledger-reports'.

Prompts the user for the name of the report to run.  If no name is
entered, the user will be prompted for a command line to run.  The
command line specified or associated with the selected report name
is run and the output is made available in another buffer for viewing.
If a prefix argument is given and the user selects a valid report
name, the user is prompted with the corresponding command line for
editing before the command is run.

The output buffer will be in `cl-ledger-report-mode', which defines
commands for saving a new named report based on the command line
used to generate the buffer, navigating the buffer, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
		(y-or-n-p "Buffer modified, save it? "))
       (save-buffer))
     (let ((rname (cl-ledger-report-read-name))
	   (edit (not (null current-prefix-arg))))
       (list rname edit))))
  (let ((buf (current-buffer))
	(rbuf (get-buffer cl-ledger-report-buffer-name))
	(wcfg (current-window-configuration)))
    (if rbuf
	(kill-buffer rbuf))
    (with-current-buffer
	(pop-to-buffer (get-buffer-create cl-ledger-report-buffer-name))
      (cl-ledger-report-mode)
      (set (make-local-variable 'cl-ledger-buf) buf)
      (set (make-local-variable 'cl-ledger-report-name) report-name)
      (set (make-local-variable 'cl-ledger-original-window-cfg) wcfg)
      (cl-ledger-do-report (cl-ledger-report-cmd report-name edit))
      (shrink-window-if-larger-than-buffer))))

(defun string-empty-p (s)
  "Check for the empty string."
  (string-equal "" s))

(defun cl-ledger-report-name-exists (name)
  "Check to see if the given report name exists.

If name exists, returns the object naming the report, otherwise returns nil."
  (unless (string-empty-p name)
    (car (assoc name cl-ledger-reports))))

(defun cl-ledger-reports-add (name cmd)
  "Add a new report to `cl-ledger-reports'."
  (setq cl-ledger-reports (cons (list name cmd) cl-ledger-reports)))

(defun cl-ledger-reports-custom-save ()
  "Save the `cl-ledger-reports' variable using the customize framework."
  (customize-save-variable 'cl-ledger-reports cl-ledger-reports))

(defun cl-ledger-report-read-command (report-cmd)
  "Read the command line to create a report."
  (read-from-minibuffer "Report command line: "
			(if (null report-cmd) "ledger " report-cmd)
			nil nil 'cl-ledger-report-cmd-prompt-history))

(defun cl-ledger-report-cl-ledger-file-format-specifier ()
  "Substitute the full path to master or current ledger file

The master file name is determined by the cl-ledger-master-file buffer-local
variable which can be set using file variables.  If it is set, it is used,
otherwise the current buffer file is used."
  (cl-ledger-master-file))

(defun cl-ledger-read-string-with-default (prompt default)
  (let ((default-prompt (concat prompt
				(if default
				    (concat " (" default "): ")
				  ": "))))
    (read-string default-prompt nil nil default)))

(defun cl-ledger-report-payee-format-specifier ()
  "Substitute a payee name

The user is prompted to enter a payee and that is substitued.  If
point is in an entry, the payee for that entry is used as the
default."
  ;; It is intended copmletion should be available on existing
  ;; payees, but the list of possible completions needs to be
  ;; developed to allow this.
  (cl-ledger-read-string-with-default "Payee" (regexp-quote (cl-ledger-entry-payee))))

(defun cl-ledger-report-account-format-specifier ()
  "Substitute an account name

The user is prompted to enter an account name, which can be any
regular expression identifying an account.  If point is on an account
transaction line for an entry, the full account name on that line is
the default."
  ;; It is intended completion should be available on existing account
  ;; names, but it remains to be implemented.
  (let* ((context (cl-ledger-context-at-point))
	 (default
	  (if (eq (cl-ledger-context-line-type context) 'acct-transaction)
	      (regexp-quote (cl-ledger-context-field-value context 'account))
	    nil)))
    (cl-ledger-read-string-with-default "Account" default)))

(defun cl-ledger-report-expand-format-specifiers (report-cmd)
  (let ((expanded-cmd report-cmd))
    (while (string-match "%(\\([^)]*\\))" expanded-cmd)
      (let* ((specifier (match-string 1 expanded-cmd))
	     (f (cdr (assoc specifier cl-ledger-report-format-specifiers))))
	(if f
	    (setq expanded-cmd (replace-match
				(save-match-data
				  (with-current-buffer cl-ledger-buf
				    (shell-quote-argument (funcall f))))
				t t expanded-cmd))
	  (progn
	    (set-window-configuration cl-ledger-original-window-cfg)
	    (error "Invalid ledger report format specifier '%s'" specifier)))))
    expanded-cmd))

(defun cl-ledger-report-cmd (report-name edit)
  "Get the command line to run the report."
  (let ((report-cmd (car (cdr (assoc report-name cl-ledger-reports)))))
    ;; logic for substitution goes here
    (when (or (null report-cmd) edit)
      (setq report-cmd (cl-ledger-report-read-command report-cmd)))
    (setq report-cmd (cl-ledger-report-expand-format-specifiers report-cmd))
    (set (make-local-variable 'cl-ledger-report-cmd) report-cmd)
    (or (string-empty-p report-name)
	(cl-ledger-report-name-exists report-name)
	(cl-ledger-reports-add report-name report-cmd)
	(cl-ledger-reports-custom-save))
    report-cmd))

(defun cl-ledger-do-report (cmd)
  "Run a report command line."
  (goto-char (point-min))
  (insert (format "Report: %s\n" cmd)
	  (make-string (- (window-width) 1) ?=)
	  "\n")
  (shell-command cmd t nil))

(defun cl-ledger-report-goto ()
  "Goto the ledger report buffer."
  (interactive)
  (let ((rbuf (get-buffer cl-ledger-report-buffer-name)))
    (if (not rbuf)
	(error "There is no ledger report buffer"))
    (pop-to-buffer rbuf)
    (shrink-window-if-larger-than-buffer)))

(defun cl-ledger-report-redo ()
  "Redo the report in the current ledger report buffer."
  (interactive)
  (cl-ledger-report-goto)
  (erase-buffer)
  (cl-ledger-do-report cl-ledger-report-cmd))

(defun cl-ledger-report-quit ()
  "Quit the ledger report buffer."
  (interactive)
  (cl-ledger-report-goto)
  (set-window-configuration cl-ledger-original-window-cfg))

(defun cl-ledger-report-kill ()
  "Kill the ledger report buffer."
  (interactive)
  (cl-ledger-report-quit)
  (kill-buffer (get-buffer cl-ledger-report-buffer-name)))

(defun cl-ledger-report-edit ()
  "Edit the defined ledger reports."
  (interactive)
  (customize-variable 'cl-ledger-reports))

(defun cl-ledger-report-read-new-name ()
  "Read the name for a new report from the minibuffer."
  (let ((name ""))
    (while (string-empty-p name)
      (setq name (read-from-minibuffer "Report name: " nil nil nil
				       'cl-ledger-report-name-prompt-history)))
    name))

(defun cl-ledger-report-save ()
  "Save the current report command line as a named report."
  (interactive)
  (cl-ledger-report-goto)
  (let (existing-name)
    (when (string-empty-p cl-ledger-report-name)
      (setq cl-ledger-report-name (cl-ledger-report-read-new-name)))

    (while (setq existing-name (cl-ledger-report-name-exists cl-ledger-report-name))
      (cond ((y-or-n-p (format "Overwrite existing report named '%s' "
			       cl-ledger-report-name))
	     (when (string-equal
		    cl-ledger-report-cmd
		    (car (cdr (assq existing-name cl-ledger-reports))))
	       (error "Current command is identical to existing saved one"))
	     (setq cl-ledger-reports
		   (assq-delete-all existing-name cl-ledger-reports)))
	    (t
	     (setq cl-ledger-report-name (cl-ledger-report-read-new-name)))))

    (cl-ledger-reports-add cl-ledger-report-name cl-ledger-report-cmd)
    (cl-ledger-reports-custom-save)))

;; In-place completion support

;; A sample function for $ users

(defun cl-ledger-align-dollars (&optional column)
  (interactive "p")
  (if (= column 1)
      (setq column 52))
  (while (search-forward "$" nil t)
    (backward-char)
    (let ((col (current-column))
	  (beg (point))
	  target-col len)
      (skip-chars-forward "-$0-9,.")
      (setq len (- (point) beg))
      (setq target-col (- column len))
      (if (< col target-col)
	  (progn
	    (goto-char beg)
	    (insert (make-string (- target-col col) ? )))
	(move-to-column target-col)
	(if (looking-back "  ")
	    (delete-char (- col target-col))
	  (skip-chars-forward "^ \t")
	  (delete-horizontal-space)
	  (insert "  ")))
      (forward-line))))

(defun cl-ledger-next-amount (&optional end)
  (when (re-search-forward "\\(  \\|\t\\| \t\\)[ \t]*-?\\([A-Z$]+ *\\)?\\(-?[0-9,]+?\\)\\(.[0-9]+\\)?\\( *[A-Z$]+\\)?\\([ \t]*@@?[^\n;]+?\\)?\\([ \t]+;.+?\\)?$" (marker-position end) t)
    (goto-char (match-beginning 0))
    (skip-syntax-forward " ")
    (- (or (match-end 4)
	   (match-end 3)) (point))))

(defun cl-ledger-align-amounts (&optional column)
  "Align amounts in the current region.
This is done so that the last digit falls in COLUMN, which defaults to 52."
  (interactive "p")
  (if (= column 1)
      (setq column 52))
  (save-excursion
    (let* ((mark-first (< (mark) (point)))
	   (begin (if mark-first (mark) (point)))
	   (end (if mark-first (point-marker) (mark-marker)))
	   offset)
      (goto-char begin)
      (while (setq offset (cl-ledger-next-amount end))
	(let ((col (current-column))
	      (target-col (- column offset))
	      adjust)
	  (setq adjust (- target-col col))
	  (if (< col target-col)
	      (insert (make-string (- target-col col) ? ))
	    (move-to-column target-col)
	    (if (looking-back "  ")
		(delete-char (- col target-col))
	      (skip-chars-forward "^ \t")
	      (delete-horizontal-space)
	      (insert "  ")))
	  (forward-line))))))

;; A sample entry sorting function, which works if entry dates are of
;; the form YYYY/mm/dd.

(defun cl-ledger-sort ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sort-subr
     nil
     (function
      (lambda ()
	(if (re-search-forward
	     (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
		     "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (point-max)))))
     (function
      (lambda ()
	(forward-paragraph))))))

;; General helper functions

(defvar cl-ledger-delete-after nil)

(defun cl-ledger-run-ledger (buffer &rest args)
  "run ledger with supplied arguments"
  (cond
   ((null cl-ledger-binary-path)
    (error "The variable `cl-ledger-binary-path' has not been set"))
   ((not (file-exists-p cl-ledger-binary-path))
    (error "The file `cl-ledger-binary-path' (\"%s\") does not exist"
	   cl-ledger-binary-path))
   ((not (file-executable-p cl-ledger-binary-path))
    (error "The file `cl-ledger-binary-path' (\"%s\") cannot be executed"
	   cl-ledger-binary-path))
   (t
    (let ((buf (current-buffer)))
      (with-current-buffer buffer
	(apply #'call-process-region
	       (append (list (point-min) (point-max)
			     cl-ledger-binary-path cl-ledger-delete-after
			     buf nil "-f" "-")
		       args)))))))

(defun cl-ledger-run-cl-ledger-and-delete (buffer &rest args)
  (let ((cl-ledger-delete-after t))
    (apply #'cl-ledger-run-ledger buffer args)))

(defun cl-ledger-set-year (newyear)
  "Set ledger's idea of the current year to the prefix argument."
  (interactive "p")
  (if (= newyear 1)
      (setq cl-ledger-year (read-string "Year: " (cl-ledger-current-year)))
    (setq cl-ledger-year (number-to-string newyear))))

(defun cl-ledger-set-month (newmonth)
  "Set ledger's idea of the current month to the prefix argument."
  (interactive "p")
  (if (= newmonth 1)
      (setq cl-ledger-month (read-string "Month: " (cl-ledger-current-month)))
    (setq cl-ledger-month (format "%02d" newmonth))))

(defvar cl-ledger-master-file nil)

(defun cl-ledger-master-file ()
  "Return the master file for a ledger file.

The master file is either the file for the current ledger buffer or the
file specified by the buffer-local variable cl-ledger-master-file.  Typically
this variable would be set in a file local variable comment block at the
end of a ledger file which is included in some other file."
  (if cl-ledger-master-file
      (expand-file-name cl-ledger-master-file)
    (buffer-file-name)))

(provide 'ledger)

;;; ledger.el ends here
