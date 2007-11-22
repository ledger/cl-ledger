;; types.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(deftype item-status ()
  '(member uncleared pending cleared))

(defstruct (item-position)
  begin-char
  end-char
  begin-line
  end-line
  source)

(defstruct (transaction
	     (:conc-name xact-)
	     (:print-function print-transaction))
  entry
  (actual-date nil     :type (or fixed-time null))
  (effective-date nil  :type (or fixed-time null))
  (status 'uncleared   :type item-status)
  account
  (amount nil	       :type (or value function null))
  (cost nil	       :type (or value function null))
  (note nil	       :type (or string null))
  (tags nil)
  (virtualp nil        :type boolean)
  (generatedp nil      :type boolean)
  (calculatedp nil     :type boolean)
  (must-balance-p t    :type boolean)
  position
  data)

(defclass entry ()
  ((journal        :accessor entry-journal	   :initarg :journal)
   (actual-date	   :accessor entry-actual-date	   :initarg :actual-date
		   :initform nil :type (or fixed-time null))
   (effective-date :accessor entry-effective-date  :initarg :effective-date
		   :initform nil :type (or fixed-time null))
   (status         :accessor entry-status	   :initarg :status
		   :initform 'uncleared :type item-status)
   (code	   :accessor entry-code		   :initarg :code
		   :initform nil :type (or string null))
   (payee	   :accessor entry-payee	   :initarg :payee
		   :initform nil :type (or string null))
   (note	   :accessor entry-note		   :initarg :note
		   :initform nil :type (or string null))
   (transactions   :accessor entry-transactions	   :initarg :transactions
		   :initform nil)
   (position       :accessor entry-position        :initarg :position
		   :initform nil)
   (normalizedp    :accessor entry-normalizedp     :initarg :normalizedp
		   :initform nil)
   (data           :accessor entry-data            :initarg :data
		   :initform nil)))

(defclass account ()
  ((parent         :accessor account-parent	   :initarg :parent
		   :initform nil)
   (children       :accessor account-children	   :initarg :children
		   :initform nil :type (or hash-table null))
   (name	   :accessor account-name	   :initarg :name
		   :type string)
   (fullname	   :accessor account-fullname	   :initarg :fullname
		   :type string)
   (transactions   :accessor account-transactions  :initarg :transactions
		   :initform nil)
   (last-transaction-cell :accessor account-last-transaction-cell :initform nil)
   (data           :accessor account-data          :initarg :data
		   :initform nil)))

(defclass journal ()
  ((binder	   :accessor journal-binder	   :initarg :binder)
   (contents       :accessor journal-contents      :initform nil)
   (last-content-cell :accessor journal-last-content-cell :initform nil)
   (entries	   :accessor journal-entries	   :initarg :entries
		   :initform nil)
   (last-entry-cell :accessor journal-last-entry-cell :initform nil)
   (date-format    :accessor journal-date-format  :initform nil
		   :type (or string null))
   (default-year   :accessor journal-default-year  :initform nil
		   :type (or integer null))
   (default-account :accessor journal-default-account :initform nil
		    :type (or account null))
   (source	   :accessor journal-source	   :initarg :source-path
		   :type pathname)
   (data           :accessor journal-data          :initarg :data
		   :initform nil)))

(defclass binder ()
  ((commodity-pool :accessor binder-commodity-pool :initarg :commodity-pool
		   :type commodity-pool)
   (root-account   :accessor binder-root-account   :initarg :root-account
		   :initform (make-instance 'account :name "") :type account)
   (journals	   :accessor binder-journals	   :initarg :journals
		   :initform nil)
   (transactions   :accessor binder-transactions   :initarg :transactions
		   :initform nil)
   (data           :accessor binder-data           :initarg :data
		   :initform nil)))

(defgeneric add-transaction (item transaction))
(defgeneric add-journal (binder journal))
(defgeneric find-account (item account-path &key create-if-not-exists-p))
(defgeneric entries-iterator (object))
(defgeneric transactions-iterator (object &optional entry-transform))

(provide 'types)

;; types.lisp ends here
