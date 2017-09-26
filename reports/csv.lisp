;; csv.lisp

(in-package :ledger)

(defun csv-reporter (&optional (output-stream *standard-output*))
  (lambda (xact)
    (let* ((entry (xact-entry xact))
           (date (strftime (xact-date xact)))
           (code (entry-code entry))
           (payee (entry-payee entry))
           (account (account-fullname (xact-account xact)))
           (amount (xact-amount xact))
           (commodity (amount-commodity amount))
           (commodity-name (commodity-name commodity))
           (precision (amount-precision amount))
           (quantity (let ((*default-display-precision* precision))
                       (format-value (amount-quantity amount))))
           (status (case (or (xact-status xact) (entry-status entry))
                     ((:cleared) "*")
                     ((:pending) "!")))
           (note (or (xact-note xact) (entry-note entry))))
      (format output-stream "~{\"~A\"~^,~}~%"
              (mapcar (lambda (item)
                        (if (stringp item)
                            (with-output-to-string (out)
                              (with-input-from-string (in item)
                                (loop for c = (read-char in nil)
                                      while c
                                      do (progn
                                           (when (char= c #\")
                                             (write-char #\\ out))
                                           (write-char c out)))))
                            ""))
                      (list date code payee account commodity-name
                            quantity status note))))))

(defun csv-report (&rest args)
  (let ((output-stream (or (cadr (member :output-stream args))
                           *standard-output*)))
    (format output-stream
            "Date,Code,Payee,Account,Commodity,Amount,Status,Note~%")
    (transactions-report
     (append args (list :reporter (csv-reporter output-stream))))))

(provide 'csv)
