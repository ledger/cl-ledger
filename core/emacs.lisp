;; emacs.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun find-current-entity (journal-path goal-line)
  (if (stringp journal-path)
      (setf journal-path (pathname journal-path)))
  (let ((entry-class (find-class 'entry)))
    (dolist (journal (binder-journals *last-binder*))
      (when (equal (journal-source journal) journal-path)
	(dolist (item (journal-contents journal))
	  (when (eq (class-of item) entry-class)
	    (let ((entry-line
		   (item-position-begin-line (entry-position item))))
	      (if (= entry-line goal-line)
		  (return-from find-current-entity item)
		  (if (and (> goal-line entry-line)
			   (<= goal-line
			      (+ entry-line
				 (length (entry-transactions item)))))
		      (return-from find-current-entity
			(nth (- goal-line entry-line 1)
			     (entry-transactions item))))))))))))

(provide 'emacs)

;; emacs.lisp ends here
