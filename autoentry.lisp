(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger.textual)

(defclass automated-entry (entry)
  ((predicate :accessor entry-predicate :initarg :predicate :type function)))

(defun read-automated-entry (journal in)
  (declare (type journal journal))
  (declare (type stream in))
  (let ((entry
	 (make-instance 'automated-entry
			:journal journal
			:predicate (parse-value-expr (read-line in)))))
    (loop
       for transaction = (read-transaction entry in)
       while transaction do
       (add-transaction entry transaction)
       (add-transaction (xact-account transaction) transaction))
    entry))

(pushnew `(#\= . ,#'(lambda (c in binder)
		      (let ((journal (binder-journal binder)))
			(add-entry journal (read-automated-entry journal in)))))
	 *directive-handlers*)

(provide 'autoentry)

;; autoentry.lisp ends here
