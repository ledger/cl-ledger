;; ledger.http.lisp

(declaim (optimize (safety 3) (debug 3)))

(defpackage :ledger.http
  (:use :common-lisp :local-time :cambl :ledger :cl-ppcre
	:hunchentoot :cl-who))

(in-package :ledger.http)

(defun print-html-balance (balance)
  (let* ((right-style "text-align: right; padding-left: 1em")
	 (red-right-style
	  (format nil "~A; color: red" right-style)))
    (with-html-output (*standard-output*)
      (:table
       :border 0 :cellpadding 0 :cellspacing 0
       (mapc #'(lambda (amount)
		 (with-html-output (*standard-output*)
		   (:tr
		    (:td :style (if (value-minusp (cdr amount))
				    red-right-style
				    right-style)
			 (print-value (cdr amount))))))
	     (get-amounts-map balance))))))

(defun string-emptyp (string)
  (string= string ""))

(defun string-if-not-empty (string)
  (and (not (string-emptyp string))
       string))

(define-easy-handler (easy-demo :uri "/"
                                :default-request-type :post)
    ((journal-path :parameter-type 'string)
     (account :parameter-type 'string)
     (payee :parameter-type 'string)
     (begin-date :parameter-type 'string)
     (end-date :parameter-type 'string)
     (value-expr :parameter-type 'string)
     (calculate-before-p :parameter-type 'boolean))
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "Ledger Register"))
     (:body
      :style "margin: 50px 0 0 50px"
      (:p (:form
	   :method :post
	   (:table
	    :border 0 :cellpadding 2 :cellspacing 1
	    (:tr
	     (:td :style "text-align: right"
		  (str "Journal path:"))
	     (:td (:input
		   :type :text
		   :style "width: 30em"
		   :name "journal-path"
		   :value (or journal-path "doc/sample.dat")))
	     (:td (:input :type :submit :value "Register")))
	    (:tr
	     (:td :style "text-align: right"
		  (str "Account:"))
	     (:td (:input
		   :type :text
		   :style "width: 30em"
		   :name "account"
		   :value (or account "")))
	     (:td "&nbsp;"))
	    (:tr
	     (:td :style "text-align: right"
		  (str "Payee:"))
	     (:td (:input
		   :type :text
		   :style "width: 30em"
		   :name "payee"
		   :value (or payee "")))
	     (:td "&nbsp;"))
	    (:tr
	     (:td :style "text-align: right"
		  (str "Begin:"))
	     (:td (:input
		   :type :text
		   :style "width: 30em"
		   :name "begin-date"
		   :value (or begin-date "")))
	     (:td "&nbsp;"))
	    (:tr
	     (:td :style "text-align: right"
		  (str "End:"))
	     (:td (:input
		   :type :text
		   :style "width: 30em"
		   :name "end-date"
		   :value (or end-date "")))
	     (:td "&nbsp;"))
	    (:tr
	     (:td :style "text-align: right"
		  (str "Expr:"))
	     (:td (:input
		   :type :text
		   :style "width: 30em"
		   :name "value-expr"
		   :value (or value-expr "")))
	     (:td "&nbsp;"))
	    (:tr
	     (:td "&nbsp;")
	     (:td (:input
		   :type :checkbox
		   :name "calculate-before-p"
		   :checked calculate-before-p)
		  (str "Calculate totals before filter"))
	     (:td "&nbsp;")))))
      (let ((binder (read-binder journal-path)))
	(when calculate-before-p
	  (setf binder (calculate-totals binder)))
	(if (or account payee begin-date end-date value-expr)
	    (setf binder
		  (apply-filter binder
				(compose-predicate
				 :account (string-if-not-empty account)
				 :payee (string-if-not-empty payee)
				 :begin (string-if-not-empty begin-date)
				 :end (string-if-not-empty end-date)
				 :expr (string-if-not-empty value-expr)))))
	(unless calculate-before-p
	  (setf binder (calculate-totals binder)))
	(htm
	 (:p
	  (:table
	   :border 1 :cellpadding 5 :cellspacing 2
	   (:thead
	    (:tr (:th "Date") (:th "Payee")
		 (:th "Account") (:th "Amount") (:th "Total")))
	   (loop
	      with iterator = (transactions-iterator binder)
	      for xact = (funcall iterator)
	      while xact do
	      (let* ((amt (xact-amount xact))
		     (right-style "text-align: right; padding-left: 1em")
		     (red-right-style
		      (format nil "~A; color: red" right-style))
		     (running-total (xact-value xact :running-total)))
		(htm
		 (:tr
		  (:td (str (strftime (xact-date xact))))
		  (:td (str (entry-payee (xact-entry xact))))
		  (:td :style "font-weight: bold"
		       (str (account-fullname (xact-account xact))))
		  (:td :style (if (value-minusp amt)
				  red-right-style
				  right-style)
		       (print-value amt))
		  (if (balancep running-total)
		      (htm
		       (:td :style right-style
			    (print-html-balance running-total)))
		      (htm
		       (:td :style (if (value-minusp running-total)
				       red-right-style
				       right-style)
			    (print-value running-total))))))))))))))))

(defun hello-world ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
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

;; ledger.http.lisp ends here
