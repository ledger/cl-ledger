;; ledger-server.lisp

(declaim (optimize (safety 3) (debug 3)))

(defpackage :ledger-server
  (:use :common-lisp :cambl :ledger :cl-ppcre :hunchentoot :cl-who))

(in-package :ledger-server)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun hello-world ()
  (with-html
    (:html
     (:head (:title "Hello, world!"))
     (:body
      (:h1 "Hello, world!")
      (:p "This is the canonical hello message.")))))

(setq *dispatch-table*
      (list (create-prefix-dispatcher "/hello" #'hello-world)
	    #'default-dispatcher))

;; ledger-server.lisp ends here
