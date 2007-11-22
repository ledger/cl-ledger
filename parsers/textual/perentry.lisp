(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger-textual)

(defclass periodic-entry (entry)
  ((period :accessor entry-period :initarg :period :initform nil)))

(defun read-periodic-entry (in journal)
  (declare (type journal journal))
  (declare (type stream in))
  (let ((period-string (read-line in nil)))
    (let ((entry
	   (make-instance 'periodic-entry
			  :journal journal
			  ;; jww (2007-11-21): This isn't working just yet
			  :period (and nil
				       ;; (periods:parse-time-period period-string)
				       ))))
      (loop
	 for transaction = (read-transaction in entry)
	 while transaction do
	 (add-transaction entry transaction))

      (let ((periodic-entries (assoc :periodic-entries
				     (journal-data journal))))
	(if periodic-entries
	    (nconc (cdr periodic-entries) (list entry))
	    (push (cons :periodic-entries (list entry))
		  (journal-data journal))))

      (add-to-contents journal entry)
      entry)))

(pushnew `(#\~ . ,#'(lambda (in journal)
		      (add-to-contents journal (read-periodic-entry in journal))))
	 *directive-handlers*)

(provide 'perentry)
