;; ledger-server.lisp

(declaim (optimize (safety 3) (debug 3)))

(defpackage :ledger-server
  (:use :common-lisp :cambl :ledger :cl-ppcre :hunchentoot :cl-who))

(in-package :ledger-server)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(define-easy-handler (easy-demo :uri "/"
                                :default-request-type :post)
    ((journal-path :parameter-type 'string))
  (with-html
      (:html
       (:head (:title "Ledger Register"))
       (:body
        :style "margin: 50px"
        (:p (:form
             :method :post
             (:table
              :border 0 :cellpadding 5 :cellspacing 0
              (:tr
               (:td :style "text-align: right"
                    (str "Journal path:"))
               (:td (:input
                     :type :text
                     :style "width: 30em"
                     :name "journal-path"
                     :value (or journal-path "sample.dat")))
               (:td (:input :type :submit :value "Register"))))))
        (when journal-path
          (let ((binder (binder journal-path)))
            (setf binder (calculate-running-totals
                          (normalize binder)))
            (dolist (journal (binder-journals binder))
              (htm
               (:p
                (:table
                 :border 1 :cellpadding 5 :cellspacing 2
                 :style "margin: 10px 0 0 5%"
                 (:thead
                  (:tr (:th "Payee") (:th "Amount") (:th "Total")))
		 (loop
		    with iterator = (entries-iterator journal)
		    for entry = (funcall iterator)
		    while entry do
		    (dolist (xact (entry-transactions entry))
		      (let ((amt (xact-amount xact)))
			(htm
			 (:tr
			  (:td (str (entry-payee entry)))
			  (:td :style "text-align: right; padding-left: 2em"
			       (print-value amt))
			  (:td
			   :style "text-align: right; padding-left: 2em"
			   (print-value
			    (cdr (assoc :running-total (xact-data xact)))
			    :line-feed-string "<br />")))))))))))))))))

(defun hello-world ()
  (with-html
    (:html
     (:head (:title "Welcome to your Finances"))
     (:body
      (:h1 "Hello, world!")
      (:p "This is the canonical hello message.")))))

(progn
  (setf *dispatch-table*
        (list #'dispatch-easy-handlers
              (create-prefix-dispatcher "/hello" #'hello-world)
              #'default-dispatcher))

  (setf *show-lisp-errors-p* t
        *show-lisp-backtraces-p* t))

;; ledger-server.lisp ends here
