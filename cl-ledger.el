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

(defcustom cl-ledger-file nil
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

(defconst +cl-ledger-transaction-regexp+
  "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)")

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

  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?a)] 'cl-ledger-add-entry)
    (define-key map [(control ?c) (control ?d)] 'cl-ledger-delete-current-entry)
    (define-key map [(control ?c) (control ?y)] 'cl-ledger-set-year)
    (define-key map [(control ?c) (control ?m)] 'cl-ledger-set-month)
    (define-key map [(control ?c) (control ?c)] 'cl-ledger-toggle-current)
    (define-key map [(control ?c) (control ?e)] 'cl-ledger-toggle-current-entry)
    (define-key map [(control ?c) (control ?r)] 'cl-ledger-reconcile)
    (define-key map [(control ?c) (control ?s)] 'cl-ledger-sort)
    (define-key map [(control ?c) (control ?t)] 'cl-ledger-total-at-point)

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

(defsubst string-without-properties (str)
  (set-text-properties 0 (length str) nil str)
  str)

(defsubst cl-ledger-render-balance (balance)
  (let ((str (mapconcat #'identity (split-string balance "\n" t)
			" / ")))
    (set-text-properties 0 (length str) (list 'face 'bold) str)
    str))

(defun cl-ledger-total-at-point ()
  (interactive)
  (let* ((account
	  (save-excursion
	    (goto-char (line-beginning-position))
	    (and (looking-at +cl-ledger-transaction-regexp+)
		 (string-without-properties (match-string 2)))))
	 (entry (slime-eval
		 `(ledger:sexp-report
		   ,(buffer-file-name (current-buffer))
		   :account ,(concat "^" account "$")
		   :limit ,(format "line > 0 & line <= %d"
				   (line-number-at-pos)) :tail 1))))
    (message "Total as of this transaction: %s"
	     (cl-ledger-render-balance (nth 4 (car (nth 4 (car entry))))))))

(defun cl-ledger-report (&optional account range report-type)
  (unless account
    (setf account
	  (or (save-excursion
		(goto-char (line-beginning-position))
		(and (looking-at +cl-ledger-transaction-regexp+)
		     (string-without-properties (match-string 2))))
	      (read-string "Account regex: "))))

  (unless range
    (setf range (read-string "Date range: " "this year")))

  (let ((filename (buffer-file-name (current-buffer))))
    (with-current-buffer
	(window-buffer
	 (display-buffer (get-buffer-create "*Ledger Report*")))
      (erase-buffer)
      (insert (slime-eval
	       `(cl:with-output-to-string
		 (str)
		 (,report-type ,filename
			       :account ,(concat "^" account "$")
			       :range ,range
			       :output-stream str)))))))

(defun cl-ledger-register (&optional account range)
  (interactive)
  (cl-ledger-report account range 'ledger:register-report))

;;;_* Context-sensitive completion

(defun cl-ledger-thing-at-point ()
  (let ((here (point)))
    (goto-char (line-beginning-position))
    (cond ((looking-at +cl-ledger-entry-regexp+)
	   (goto-char (match-beginning 3))
	   'entry)
	  ((looking-at "^\\s-+\\([*!]\\s-+\\)?[[(]?\\(.\\)")
	   (goto-char (match-beginning 2))
	   'transaction)
	  (t
	   (ignore (goto-char here))))))

(defun cl-ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (save-excursion
   (let* ((end (point))
	  (info (cons (cl-ledger-thing-at-point) (point)))
	  (begin (cdr info))
	  begins args)
     (when (< (point) end)
       (skip-chars-forward " \t\n")
       (setq begins (cons (point) begins))
       (setq args (cons (buffer-substring-no-properties
			 (car begins) end)
			args)))
     (cons (reverse args) (reverse begins)))))

(defun cl-ledger-complete-at-point ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (let ((filename (buffer-file-name (current-buffer)))
	(current-arg (caar (cl-ledger-parse-arguments))))
    (while (pcomplete-here
	    (if (save-excursion
		  (eq (cl-ledger-thing-at-point) 'entry))
		(slime-eval `(ledger:find-unique-payees
			      ,filename :starts-with ,current-arg))
	      (slime-eval `(ledger:find-sibling-accounts
			    ,filename :path ,current-arg)))
	    (let ((pos (position ?: current-arg :from-end t)))
	      (if pos
		  (substring current-arg (1+ pos))
		current-arg))))))

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
  (progn
    (slime-eval
     `(cl:setf (,function (ledger:find-current-entity
			   ,(buffer-file-name (current-buffer)) ,line))
	       ,value))))

(defun cl-ledger-toggle-current (&optional style)
  (interactive)
  (if (or cl-ledger-clear-whole-entries
	  (save-excursion
	    (eq 'entry (cl-ledger-thing-at-point))))
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
	 (progn
	   (slime-eval
	    `(ledger:sexp-report ,(buffer-file-name cl-ledger-buf)
				 :account ,cl-ledger-acct
				 :display "!X")))))
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

(defun cl-ledger-reconcile-isearch-backward ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (switch-to-buffer-other-window cl-ledger-buf)
    (goto-char (cdr where))
    (call-interactively 'isearch-backward)))

(defun cl-ledger-reconcile-update-mode-string ()
  (let* ((entries (progn
		    (slime-eval
		     `(ledger:sexp-report
		       ,(buffer-file-name cl-ledger-buf)
		       :account ,cl-ledger-acct
		       :not-status :uncleared :tail 1))))
	 (cleared-total (nth 4 (car (nth 4 (car entries))))))
    ;; jww (2007-12-05): What about when the cleared-total is a balance?
    (setf mode-name
	  (if cleared-total
	      (let ((difference
		     (progn
		       (slime-eval
			`(cl:let ((diff (cambl:subtract
					 (cambl:amount* ,cl-ledger-goal)
					 (cambl:amount* ,cleared-total))))
				 (cl:unless (cambl:value-zerop diff)
					    (cambl:format-value diff)))))))

		(if difference
		    (format "Reconcile:%s/%s" cleared-total difference)
		  (let ((str (concat "Reconcile:" cleared-total)))
		    (add-text-properties 0 (length str)
					 (list 'face bold) str)
		    str)))
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
    (define-key map [(control ?r)] 'cl-ledger-reconcile-isearch-backward)
    (define-key map [? ] 'cl-ledger-reconcile-toggle)
    (define-key map [space] 'cl-ledger-reconcile-toggle)
    (define-key map [?a] 'cl-ledger-reconcile-add)
    (define-key map [?d] 'cl-ledger-reconcile-delete)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line)
    (define-key map [?s] 'cl-ledger-reconcile-save)
    (define-key map [?q] 'cl-ledger-reconcile-quit)
    (use-local-map map)))

;;;_* A command-line interface to CL-Ledger, in the style of 2.6

(defun cl-ledger-eval (command &rest args)
  (progn
    (slime-eval
     `(cl:with-output-to-string
       (str)
       (,command ,(expand-file-name cl-ledger-file)
		 ,@args
		 :output-stream str)))))

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

  ;; Strip all text properties from the arguments
  (setf args (mapcar #'(lambda (arg)
			 (if (stringp arg)
			     (set-text-properties 0 (length arg) nil arg))
			 arg) args))

  (let (keywords)
    ;; Handle all of the option-like arguments
    (while (and args
		(plusp (length (first args)))
		(char-equal ?- (aref (first args) 0)))
      (cond
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
      (setq args (cdr args))

      ;; Extract the account and payee regexps
      (dolist (arg args)
	(if (string= arg "--")
	    (setq in-payee-regexps t)
	  (if in-payee-regexps
	      (if (char-equal ?- (aref arg 0))
		  (push (substring arg 1) not-payee-regexps)
		(push arg payee-regexps))
	    (if (char-equal ?- (aref arg 0))
		(push (substring arg 1) not-account-regexps)
	      (push arg account-regexps)))))

      (setf account-regexps (regexp-opt account-regexps)
	    not-account-regexps (regexp-opt not-account-regexps)
	    payee-regexps (regexp-opt payee-regexps)
	    not-payee-regexps (regexp-opt not-payee-regexps))

      (setf keywords
	    (append (and (not (string-emptyp account-regexps))
			 (list :account account-regexps))
		    (and (not (string-emptyp not-account-regexps))
			 (list :not-account not-account-regexps))
		    (and (not (string-emptyp payee-regexps))
			 (list :payee payee-regexps))
		    (and (not (string-emptyp not-payee-regexps))
			 (list :not-payee not-payee-regexps))
		    keywords))

      ;; Execute the command
      (cond ((or (string= "reg" command)
		 (string= "register" command))
	     (apply #'cl-ledger-eval 'ledger:register-report keywords))

	    ((or (string= "pr" command)
		 (string= "print" command))
	     (apply #'cl-ledger-eval 'ledger:print-report keywords))

	    ((or (string= "bal" command)
		 (string= "balance" command))
	     (apply #'cl-ledger-eval 'ledger:balance-report keywords))))))

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
	 (if (cl-ledger-time-less-p moment date)
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
  (dolist (entry
	   (progn
	     (slime-eval
	      `(ledger:register-sexp
		,(buffer-file-name (current-buffer))))))
    (apply callback (butlast entry (- (length entry) 4)))))

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
