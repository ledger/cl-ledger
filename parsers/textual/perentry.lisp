(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger-textual)

(defclass periodic-entry (entry)
  ((period :accessor entry-period :initarg :period :initform nil)))

(defun read-periodic-entry (in line journal)
  (declare (type stream in))
  (declare (type journal journal))
  (let ((period-string (read-line in nil))
	(lines 1))
    (let ((entry
	   (make-instance 'periodic-entry
			  :journal journal
			  ;; jww (2007-11-21): This isn't working just yet
			  :period (periods:parse-time-period period-string))))
      (loop
	 for transaction = (read-transaction in (+ line lines) entry)
	 while transaction do
	 (add-transaction entry transaction)
	 (incf lines))
      (incf lines)

      (let ((periodic-entries (assoc :periodic-entries
				     (journal-data journal))))
	(if periodic-entries
	    (nconc (cdr periodic-entries) (list entry))
	    (push (cons :periodic-entries (list entry))
		  (journal-data journal))))

      (values entry lines))))

(pushnew `(#\~ . ,#'(lambda (in line journal)
		      (multiple-value-bind (entry lines)
			  (read-periodic-entry in line journal)
			(if entry
			    (add-to-contents journal entry)
			    (error "Failed to read entry at line ~D~%" line))
			lines)))
	 *directive-handlers*)

(provide 'perentry)
