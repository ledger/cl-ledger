;; cambl.lisp

;; This is the Commoditized AMounts and BaLances library.

;; TODO:
;; - Create a function for calculating a conversion, which will
;;   automatically preserve annotation details based on context:
;;     (amount-exchange "100 DM" "$100.00" &optional datetime "Note")
;;   This will "exchange" 100 DM for $100.00, at the given datetime with
;;   the given note.

;; Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;;
;; - Neither the name of New Artisans LLC nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This library aims to provide convenient access to commoditized math.  That
;; is, math involving numbers with units.  However, unlike scientific units,
;; this library does not allow the use of "compound" units.  If you divide 1kg
;; by 1m, you do not get "1 kg/m", but an error.  This is because the intended
;; use of this library is for situations that do not allow compounded units,
;; such as financial transactions, and the algorithms have been simplified
;; accordingly.  It also allows contextual information to be tracked during
;; commodity calculations -- the time of conversion from one type to another,
;; the rated value at the time of conversion.

(declaim (optimize (debug 3)))

(defpackage :cambl
  (:use :common-lisp :rbt)
  (:export make-commodity-pool
	   *default-commodity-pool*
	   create-commodity
	   commodity-error
	   amount
	   amount-error
	   amount-commodity
	   copy-amount
	   copy-value
	   float-to-amount
	   integer-to-amount
	   exact-amount
	   parse-amount
	   parse-amount*
	   read-amount
	   read-amount*
	   print-value
	   format-value
	   display-precision
	   value=
	   value/=
	   value-equal
	   value<
	   value<=
	   value>
	   value>=
	   add
	   add*
	   subtract
	   subtract*
	   multiply
	   multiply*
	   divide
	   divide*
	   *european-style*
	   *amount-stream-fullstrings*))

(in-package :CAMBL)

(defvar *european-style* nil
  "If set to T, amounts will be printed as 1.000,00.
  The default is US style, which is 1,000.00.  Note that thousand markers are
  only used if the commodity's own `thousand-marks-p' accessor returns T.")

;; Commodity symbols

(deftype datetime ()
  'integer)

(defstruct commodity-symbol
  (name "" :type string)
  (needs-quoting-p nil :type boolean)
  (prefixed-p nil :type boolean)
  (connected-p nil :type boolean))

(defclass basic-commodity ()
  ((symbol :initarg :symbol :type commodity-symbol)
   name
   note
   smaller-unit-equivalence
   larger-unit-equivalence
   (thousand-marks-p :initarg :thousand-marks-p :initform nil
		     :type boolean)
   (no-market-price-p :initarg :no-market-price-p :initform nil
		      :type boolean)
   (builtin-p :initarg :builtin-p :initform nil :type boolean)
   (display-precision :initform 0 :type fixnum)
   price-history
   (last-lookup :type datetime)))

(defstruct commodity-pool
  (commodities-by-name-map (make-hash-table :test 'equal) :type hash-table)
  (commodities-by-serial-list '((0 . nil)) :type list)
  (default-commodity nil))

(defparameter *default-commodity-pool* (make-commodity-pool))

;; Commodities are references to basic commodities, which store the common
;; details.  This is so the commodities USD and $ can both refer to the same
;; underlying kind.

(defclass commodity ()
  ((basic-commodity :initarg :base :type basic-commodity)
   (commodity-pool :accessor parent-pool :initarg :pool
		   :type commodity-pool)
   (serial-number :accessor commodity-serial-number :type fixnum)
   (qualified-name :initarg :qualified-name :type string)
   ;; This is set automatically by initialize-instance whenever an
   ;; annotated-commodity is created.
   (annotated-p :initform nil :type boolean)))

(defmethod print-object ((commodity commodity) stream)
  (print-unreadable-object (commodity stream :type t)
    (princ (commodity-symbol commodity) stream)))

(defclass amount ()
  ((commodity :accessor amount-commodity :initarg :commodity
	      :initform nil :type (or commodity null))
   (quantity :initarg :quantity :type integer)
   (internal-precision :initarg :precision :type fixnum)
   (keep-precision :initform nil :type boolean)
   ;;value-origins-list
   ;; (commodity-pool :accessor amount-commodity-pool :initarg :pool
   ;;   :type commodity-pool)
   (keep-base :allocation :class :initform nil :type boolean)
   (keep-price :allocation :class :initform nil :type boolean)
   (keep-date :allocation :class :initform nil :type boolean)
   (keep-tag :allocation :class :initform nil :type boolean)))

(defmethod print-object ((amount amount) stream)
  (print-unreadable-object (amount stream :type t)
    (princ (concatenate 'string "\"" (format-value amount) "\"") stream)))

(defstruct pricing-entry
  (moment nil :type datetime)
  (price nil :type amount))

(defvar *amount-stream-fullstrings* nil)

(defstruct commodity-annotation
  (price nil :type (or amount null))
  (date nil :type (or datetime null))
  (tag nil :type (or string null)))

(defclass annotated-commodity (commodity)
  ((referent-commodity :initarg :referent :type commodity)
   (annotation :initarg :details :type commodity-annotation)))

(defclass balance ()
  (amounts-map))

(defgeneric commodity-base (commodity))
(defgeneric commodity-symbol (commodity))
(defgeneric commodity-equal (left right))
(defgeneric commodity-equalp (left right))
(defgeneric commodity-thousand-marks-p (commodity))
(defgeneric commodity-no-market-price-p (commodity))
(defgeneric commodity-builtin-p (commodity))
(defgeneric display-precision (item))
(defgeneric market-value (commodity &optional datetime))
(defgeneric strip-annotations (commodity &key keep-price keep-date keep-tag))
(defgeneric commodity-annotated-p (item))
(defgeneric commodity-annotation (item))
(defgeneric commodity-annotation-empty-p (annotation))
(defgeneric commodity-annotation-equal (item item))
(defgeneric annotate-commodity (commodity annotation))
(defgeneric copy-value (value))
(defgeneric print-value (value &key output-stream omit-commodity-p full-precision-p))
(defgeneric format-value (value))
(defgeneric negate* (value))
(defgeneric negate (value))
(defgeneric zero-p (amount))
(defgeneric real-zero-p (amount))
(defgeneric value= (left right))
(defgeneric value/= (left right))
(defgeneric value-equal (left right))
(defgeneric value< (left right))
(defgeneric value<= (left right))
(defgeneric value> (left right))
(defgeneric value>= (left right))
(defgeneric add-to-balance (balance value))
(defgeneric add (left right))
(defgeneric add* (left right))
(defgeneric subtract (left right))
(defgeneric subtract* (left right))
(defgeneric multiply (left right))
(defgeneric multiply* (left right))
(defgeneric divide (left right))
(defgeneric divide* (left right))
(defgeneric absolute (value))
(defgeneric round-to-precision (value &optional precision))
(defgeneric round-to-precision* (value &optional precision))
(defgeneric unround (value))
(defgeneric smallest-units* (value))
(defgeneric smallest-units (value))
(defgeneric larger-units* (value))
(defgeneric larger-units (value))
(defgeneric convert-to-amount (value))
(defgeneric convert-to-string (value))
(defgeneric convert-to-fullstring (value))
(defgeneric amount-in-balance (balance commodity))

;;; Functions:

(defmacro define-array-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-array-constant +invalid-symbol-chars+
    #(#|        0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f |#
      #| 00 |# nil nil nil nil nil nil nil nil nil  t   t  nil nil  t  nil nil
      #| 10 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| 20 |#  t   t   t  nil nil nil  t  nil  t   t   t   t   t   t   t   t 
      #| 30 |#  t   t   t   t   t   t   t   t   t   t   t   t   t   t   t   t 
      #| 40 |#  t  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| 50 |# nil nil nil nil nil nil nil nil nil nil nil  t  nil  t   t  nil
      #| 60 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| 70 |# nil nil nil nil nil nil nil nil nil nil nil  t  nil  t   t  nil
      #| 80 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| 90 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| a0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| b0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| c0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| d0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| e0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      #| f0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
  "The invalid commodity symbol characters are:
     Space Tab Newline Return
     0-9 . , ; - + * / ^ ? : & | ! = \"
     < > { } [ ] ( ) @")

(defun symbol-char-invalid-p (c)
  (let ((code (char-code c)))
    (and (< code 256)
	 (aref +invalid-symbol-chars+ code))))

(defun symbol-name-needs-quoting-p (name)
  "Return T if the given symbol NAME requires quoting."
  (declare (type string name))
  (loop for c across name do
       (and (symbol-char-invalid-p c)
	    (return t))))

(defun get-input-stream (&optional in)
  (declare (type (or stream string null) in))
  (if in
      (if (typep in 'stream)
	  in
	  (make-string-input-stream in))
      *standard-input*))

(define-condition commodity-error (error) 
  ((description :reader error-description :initarg :msg))
  (:report (lambda (condition stream)
	     (format stream "~S" (error-description condition)))))

(defun read-commodity-symbol (&optional in)
  "Parse a commodity symbol from the input stream IN.
  This is the correct entry point for creating a new commodity symbol.

  A commodity contain any character not found in `+invalid-symbol-chars+'.
  To include such characters in a symbol name -- except for #\\\", which may
  never appear in a symbol name -- surround the commodity name with double
  quotes.  It is an error if EOF is reached without reading the ending double
  quote.  If the symbol name is not quoted, and an invalid character is
  reading, reading from the stream stops and the invalid character is put
  back."
  (declare (type (or stream string null) in))

  (let ((buf (make-string-output-stream))
	(in (get-input-stream in))
	needs-quoting-p)
    (if (char= #\" (peek-char nil in))
	(progn
	  (read-char in)
	  (do ((c (read-char in) (read-char in nil 'the-end)))
	      (nil)
	    (if (characterp c)
		(if (char= #\" c)
		    (return)
		    (progn
		      (if (symbol-char-invalid-p c)
			  (setf needs-quoting-p t))
		      (write-char c buf)))
		(error 'commodity-error
		       :msg "Quoted commodity symbol lacks closing quote"))))
	(do ((c (read-char in) (read-char in nil 'the-end)))
	    ((not (characterp c)))
	  (if (symbol-char-invalid-p c)
	      (progn
		(unread-char c in)
		(return))
	      (write-char c buf))))
    (make-commodity-symbol :name (get-output-stream-string buf)
			   :needs-quoting-p needs-quoting-p)))

;; The commodity and annotated-commodity classes are the main interface class
;; for dealing with commodities themselves (which most people will never do).

(defmethod commodity-base ((commodity commodity))
  (slot-value commodity 'basic-commodity))

(defmethod commodity-symbol ((commodity commodity))
  (slot-value (slot-value commodity 'basic-commodity) 'symbol))

(defmethod commodity-thousand-marks-p ((commodity commodity))
  (slot-value (slot-value commodity 'basic-commodity) 'thousand-marks-p))

(defmethod display-precision ((commodity commodity))
  (slot-value (slot-value commodity 'basic-commodity) 'display-precision))

(defmethod display-precision ((amount amount))
  (display-precision (amount-commodity amount)))

(defmethod commodity-equal ((a commodity) (b null))
  nil)

(defmethod commodity-equal ((a null) (b commodity))
  nil)

(defmethod commodity-equal ((a null) (b null))
  t)

(defmethod commodity-equal ((a commodity) (b commodity))
  "Two commodities are considered EQUALP if they refer to the same base."
  (assert (nth-value 0 (subtypep (type-of a) 'commodity)))
  (assert (nth-value 0 (subtypep (type-of b) 'commodity)))
  (eq (slot-value a 'basic-commodity) (slot-value b 'basic-commodity)))

(defmethod commodity-equalp ((a commodity) (b null))
  nil)

(defmethod commodity-equalp ((a null) (b commodity))
  nil)

(defmethod commodity-equalp ((a commodity) (b commodity))
  "Two commodities are considered EQUALP if they refer to the same base."
  (assert (nth-value 0 (subtypep (type-of a) 'commodity)))
  (assert (nth-value 0 (subtypep (type-of b) 'commodity)))
  (eq (slot-value a 'basic-commodity) (slot-value b 'basic-commodity)))

(defun commodity-representation-lessp (left right)
  "Return T if commodity LEFT should be sorted before RIGHT."
  (declare (type (or amount commodity annotated-commodity) left))
  (declare (type (or amount commodity annotated-commodity) right))
  (block nil
    (let ((left-commodity (if (typep left 'amount)
			      (amount-commodity left)
			      left))
	  (right-commodity (if (typep right 'amount)
			       (amount-commodity right)
			       right)))

      (if (and (null left-commodity)
	       right-commodity)
	  (return t))
      (if (and left-commodity
	       (null right-commodity))
	  (return t))

      (unless (commodity-equal left-commodity right-commodity)
	(return (string-lessp (commodity-symbol-name
			       (commodity-symbol left-commodity))
			      (commodity-symbol-name
			       (commodity-symbol right-commodity)))))

      (if (and (not (commodity-annotated-p left-commodity))
	       (commodity-annotated-p right-commodity))
	  (return t))

      (if (and (commodity-annotated-p left-commodity)
	       (not (commodity-annotated-p right-commodity)))
	  (return nil))

      (let ((left-annotation (commodity-annotation left-commodity))
	    (right-annotation (commodity-annotation right-commodity)))

	(let ((left-price (commodity-annotation-price left-annotation))
	      (right-price (commodity-annotation-price right-annotation)))
	  (if (and (not left-price) right-price)
	      (return t))

	  (if (and left-price (not right-price))
	      (return nil))

	  (when (and left-price right-price)
	    (setq left-price (smallest-units left-price)
		  right-price (smallest-units right-price))

	    (if (commodity-equal (amount-commodity left-price)
				 (amount-commodity right-price))
		(return (amount-lessp left-price right-price)))

	    ;; Since we have two different amounts, there's really no way to
	    ;; establish a true sorting order; we'll just do it based on the
	    ;; numerical values.
	    (return (amount-lessp (amount-quantity left)
				  (amount-quantity right)))))

	(let ((left-date (commodity-annotation-date left-annotation))
	      (right-date (commodity-annotation-date right-annotation)))
	  (if (and (not left-date) right-date)
	      (return t))

	  (if (and left-date (not right-date))
	      (return nil))

	  (when (and left-date right-date)
	    (return (< left-date right-date))))

	(let ((left-tag (commodity-annotation-tag left-annotation))
	      (right-tag (commodity-annotation-tag right-annotation)))
	  (if (and (not left-tag) right-tag)
	      (return t))

	  (if (and left-tag (not right-tag))
	      (return nil))

	  (when (and left-tag right-tag)
	    (return (string-lessp left-tag right-tag))))))

    (return t)))

;; Routines for accessing the historical prices of a commodity

(defun add-price (commodity price datetime)
  (declare (type (or commodity annotated-commodity) commodity))
  (declare (type amount price))
  (declare (type datetime datetime))
  (let ((base (commodity-base commodity))
	(pricing-entry (make-pricing-entry :moment datetime :price price)))
    (if (not (slot-boundp base 'price-history))
	(setf (slot-value base 'price-history) (rbt:nil-tree)))
    (let ((history (slot-value base 'price-history)))
      (multiple-value-bind (new-root node-inserted-or-found item-already-in-p)
	  (rbt:insert-item pricing-entry history :key 'pricing-entry-moment)
	(if item-already-in-p
	    (setf (pricing-entry-price node-inserted-or-found) price))
	(setf (slot-value base 'price-history) new-root)))
    price))

(defun remove-price (commodity datetime)
  (declare (type (or commodity annotated-commodity) commodity))
  (declare (type datetime datetime))
  (let ((base (commodity-base commodity)))
    (when (slot-boundp base 'price-history)
      (multiple-value-bind (new-root node-deleted-p)
	  (rbt:delete-item datetime (slot-value base 'price-history)
			   :key 'pricing-entry-moment)
	(setf (slot-value base 'price-history) new-root)
	node-deleted-p))))

(defun find-nearest (it root &key (test #'<=) (key #'identity))
  "Find an item in the tree which is closest to IT according to TEST.
  For the default, <=, this means no other item will be more less than
  IT in the tree than the one found."
  (loop with p = root
     with last-found = nil
     named find-loop
     finally (return-from find-loop
	       (and last-found (rbt:node-item last-found)))
     while (not (rbt:rbt-null p))
     do
     (if (funcall test (funcall key (rbt:node-item p)) it)
	 (progn
	   ;; If the current item meets the test, it may be the one we're
	   ;; looking for.  However, there might be something closer to the
	   ;; right -- though definitely not to the left.
	   (setq last-found p
		 p (rbt:right p)))
	 ;; If the current item does not meet the test, there might be a
	 ;; candidate to the left -- but definitely not the right.
	 (setq p (rbt:left p)))))

(defmethod market-value ((commodity commodity) &optional datetime)
  (declare (type (or commodity annotated-commodity) commodity))
  (declare (type (or datetime null) datetime))
  (let ((base (commodity-base commodity)))
    (when (slot-boundp base 'price-history)
      (let ((history (slot-value base 'price-history)))
	(when history
	  (if (null datetime)
	      (progn
		(loop while (not (rbt:rbt-null (rbt:right history)))
		     do (setq history (rbt:right history)))
		(assert history)
		(rbt:node-item history))
	      (find-nearest datetime history :key 'pricing-entry-moment))))))

  ;; jww (2007-10-20): NYI
  
  ;; if (! has_flags(COMMODITY_STYLE_NOMARKET) && parent().get_quote) {
  ;;   if (optional<amount_t> quote = parent().get_quote
  ;;       (*this, age, moment,
  ;;        (base->history && base->history->prices.size() > 0 ?
  ;;         (*base->history->prices.rbegin()).first : optional<moment_t>())))
  ;;     return *quote;
  ;; }
  ;; return price;
  )

(defun get-price-quote (symbol &optional datetime)
  (assert symbol)
  (assert datetime)
  (format t "I don't know how to download prices yet."))

;; annotated-commodity's are references to other commodities (which in turn
;; reference a basic-commodity) which carry along additional contextual
;; information relating to a point in time.

(defmethod initialize-instance :after
    ((annotated-commodity annotated-commodity) &key)
  (setf (slot-value annotated-commodity 'annotated-p) t))

(defmethod commodity-base ((annotated-commodity annotated-commodity))
  (slot-value (slot-value annotated-commodity 'referent-commodity)
	      'basic-commodity))

(defmethod strip-annotations ((amount amount)
			      &key keep-price keep-date keep-tag)
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error
	   :msg "Cannot strip commodity annotations from an uninitialized amount"))

  (let ((commodity (amount-commodity amount)))
    (if (or (null commodity)
	    (not (commodity-annotated-p commodity))
	    (and keep-price keep-date keep-tag))
	amount
	(let ((tmp (copy-amount amount)))
	  (setf (slot-value tmp 'commodity)
		(strip-annotations commodity :keep-price keep-price
				   :keep-date keep-date :keep-tag keep-tag))
	  tmp))))

(defmethod strip-annotations ((commodity commodity)
			      &key keep-price keep-date keep-tag)
  (declare (ignore keep-price))
  (declare (ignore keep-date))
  (declare (ignore keep-tag))
  (assert (not (commodity-annotated-p commodity)))
  commodity)

(defmethod strip-annotations ((annotated-commodity annotated-commodity)
			      &key keep-price keep-date keep-tag)
  (assert (commodity-annotated-p annotated-commodity))
  (let* ((annotation (commodity-annotation annotated-commodity))
	 (price (commodity-annotation-price annotation))
	 (date (commodity-annotation-date annotation))
	 (tag (commodity-annotation-tag annotation)))
    (if (and (or keep-price (null price))
	     (or keep-date (null date))
	     (or keep-tag (null tag)))
	annotated-commodity
	(let ((tmp (make-instance 'annotated-commodity)))
	  (setf (slot-value tmp 'referent-commodity)
		(slot-value annotated-commodity 'referent-commodity))
	  (let ((new-ann (make-commodity-annotation)))
	    (setf (slot-value tmp 'annotation) new-ann)
	    (if keep-price
		(setf (commodity-annotation-price new-ann) price))
	    (if keep-date
		(setf (commodity-annotation-price new-ann) date))
	    (if keep-tag
		(setf (commodity-annotation-price new-ann) tag)))
	  tmp))))

(defmethod commodity-symbol ((annotated-commodity annotated-commodity))
  (commodity-symbol (slot-value annotated-commodity 'referent-commodity)))

(defmethod commodity-thousand-marks-p ((annotated-commodity annotated-commodity))
  (commodity-thousand-marks-p
   (slot-value annotated-commodity 'referent-commodity)))

(defmethod display-precision ((annotated-commodity annotated-commodity))
  (display-precision (slot-value annotated-commodity 'referent-commodity)))

(defmethod market-value ((annotated-commodity annotated-commodity) &optional datetime)
  (market-value (slot-value annotated-commodity 'referent-commodity) datetime))

(defmethod commodity-annotation-empty-p ((annotation commodity-annotation))
  (not (or (commodity-annotation-price annotation)
	   (commodity-annotation-date annotation)
	   (commodity-annotation-tag annotation))))

(defmethod commodity-annotation-equal ((a commodity-annotation)
				       (b commodity-annotation))
  (let ((price-a (commodity-annotation-price a))
	(price-b (commodity-annotation-price b))
	(date-a (commodity-annotation-date a))
	(date-b (commodity-annotation-date b))
	(tag-a (commodity-annotation-tag a))
	(tag-b (commodity-annotation-tag b)))
    (and (or (and (null price-a) (null price-b))
	     (and price-a price-b
		  (value= price-a price-b)))
	 (or (and (null date-a) (null date-b))
	     (and date-a date-b
		  (= date-a date-b)))
	 (or (and (null tag-a) (null tag-b))
	     (and tag-a tag-b
		  (string= tag-a tag-b))))))

(defun read-until (in char &optional error-message)
  (let ((text (make-string-output-stream)))
    (do ((c (read-char in nil 'the-end)
	    (read-char in nil 'the-end)))
	((if (characterp c)
	     (char= char c)
	     (prog1
		 t
	       (if error-message
		   (error 'amount-error :msg error-message)))))
      (write-char c text))
    (get-output-stream-string text)))

(defun read-commodity-annotation (in)
  (declare (type stream in))
  (let ((annotation (make-commodity-annotation)))
    (do ((c (peek-char t in nil 'the-end)
	    (peek-char t in nil 'the-end)))
	((not (characterp c)))

      (cond ((char= #\{ c)
	     (if (commodity-annotation-price annotation)
		 (error 'amount-error :msg
			"Commodity annotation specifies more than one price"))

	     (read-char in)
	     (let* ((price-string
		     (read-until in #\} "Commodity price lacks closing brace"))
		    (tmp-amount (parse-amount* price-string)))

	       ;; jww (2007-10-20): Reduce here, if need be
	       ;; temp.in_place_reduce();

	       ;; Since this price will maintain its own precision, make sure
	       ;; it is at least as large as the base commodity, since the user
	       ;; may have only specified {$1} or something similar.
	       (let ((commodity (amount-commodity tmp-amount)))
		 (if (and commodity
			  (< (slot-value tmp-amount 'internal-precision)
			     (display-precision commodity)))
		     (setq tmp-amount
			   (round-to-precision* tmp-amount
						(display-precision commodity)))))

	       (setf (commodity-annotation-price annotation) tmp-amount)))

	    ((char= #\[ c)
	     (if (commodity-annotation-date annotation)
		 (error 'amount-error :msg
			"Commodity annotation specifies more than one date"))

	     ;; jww (2007-10-20): This code cannot work until I have a decent
	     ;; Date/Time library for Common Lisp.

	     (read-char in)
	     ;;(let* ((date-string
	     ;;	    (read-until in #\] "Commodity date lacks closing bracket"))
	     ;;	   (tmp-date (parse-datetime date-string)))
	     ;;  (setf (commodity-annotation-date annotation) tmp-date))
	     )

	    ((char= #\( c)
	     (if (commodity-annotation-tag annotation)
		 (error 'amount-error :msg
			"Commodity annotation specifies more than one tag"))

	     (read-char in)
	     (setf (commodity-annotation-tag annotation)
		   (read-until in #\) "Commodity tag lacks closing parenthesis")))

	    (t
	     (return))))
    annotation))

(defun format-commodity-annotation (annotation &key
				    (output-stream *standard-output*))
  "Return the canonical annotation string for ANNOTATION.
  A fully annotated commodity always follows the form:

    [-]SYMBOL <VALUE> {PRICE} [DATE] (TAG)
  or
    <VALUE> SYMBOL {PRICE} [DATE] (TAG)

  If a particular annotation is not present, those sections is simply dropped
  from the string, for example:

    <VALUE> SYMBOL {PRICE}"
  (declare (type commodity-annotation annotation))
  (format output-stream "~:[~; {~:*~A}~]~:[~; [~:*~A]~]~:[~; (~:*~A)~]"
	  (format-value (commodity-annotation-price annotation))
	  (commodity-annotation-date annotation)
	  (commodity-annotation-tag annotation)))

(defmethod commodity-equal ((a annotated-commodity) (b annotated-commodity))
  (and (eq (slot-value a 'referent-commodity)
	   (slot-value b 'referent-commodity))
       (equal (commodity-annotation a)
	      (commodity-annotation b))))

(defmethod commodity-equal ((a commodity) (b annotated-commodity))
  nil)

(defmethod commodity-equal ((a annotated-commodity) (b commodity))
  nil)

(defmethod commodity-equalp ((a annotated-commodity) (b annotated-commodity))
  (eq (slot-value a 'referent-commodity)
      (slot-value b 'referent-commodity)))

(defmethod commodity-equalp ((a commodity) (b annotated-commodity))
  (eq a (slot-value b 'referent-commodity)))

(defmethod commodity-equalp ((a annotated-commodity) (b commodity))
  (eq (slot-value a 'referent-commodity) b))

;; All commodities are allocated within a pool, which can be used to look them
;; up.

(defun create-commodity (name &key (pool *default-commodity-pool*))
  "Create a COMMODITY after the symbol name found by parsing NAME.
  The NAME can be either a string or an input stream, or nil, in which
  case the name is read from *standard-input*.
  The argument :pool specifies the commodity pool which will maintain
  this commodity, and by which other code may access it again.
  The resulting COMMODITY object is returned."
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (let* ((symbol (if (stringp name)
		     (read-commodity-symbol name)
		     name))
	 (base (make-instance 'basic-commodity :symbol symbol))
	 (commodity (make-instance 'commodity :base base :pool pool))
	 (symbol-name (commodity-symbol-name symbol)))

    (if (commodity-symbol-needs-quoting-p symbol)
	(setf (slot-value commodity 'qualified-name)
	      (concatenate 'string "\"" symbol-name "\"")))

    (let ((commodities-by-serial-list
	   (commodity-pool-commodities-by-serial-list pool)))
      (setf (commodity-serial-number commodity)
	    (1+ (caar (last commodities-by-serial-list))))
      (nconc commodities-by-serial-list
	     (list (cons (commodity-serial-number commodity)
			 commodity))))

    (let ((names-map (commodity-pool-commodities-by-name-map pool)))
      (assert (not (gethash symbol-name names-map)))
      (setf (gethash symbol-name names-map) commodity))))

(defun find-commodity (name &key (create-if-not-exists-p nil)
		       (pool *default-commodity-pool*))
  "Find a COMMODITY identifier by the symbol name found by parsing NAME.
  The NAME can be either a string or an input stream, or nil, in which
  case the name is read from *standard-input*.

  The argument :POOL specifies the commodity pool which will maintain
  this commodity, and by which other code may access it again.

  The argument :CREATE-IF-NOT-EXISTS-P indicates whether a new commodity
  should be created if one cannot already be found.

  The return values are: COMMODITY or NIL, NEWLY-CREATED-P"
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (let ((by-name-map (commodity-pool-commodities-by-name-map pool)))
    (multiple-value-bind (entry present-p)
	(gethash (if (stringp name)
		     name
		     (commodity-symbol-name name)) by-name-map)
      (if present-p
	  (values entry nil)
	  (if create-if-not-exists-p
	      (values
	       (create-commodity name :pool pool)
	       t)
	      (values nil nil))))))

(defun find-commodity-by-serial (serial &key (pool *default-commodity-pool*))
  "Find the commodity with the matching unique SERIAL number.
  nil is returned if no such commodity exists."
  (declare (type fixnum serial))
  (declare (type commodity-pool pool))
  (let ((commodities-by-serial-list
	 (commodity-pool-commodities-by-serial-list pool)))
    (cdr (assoc serial commodities-by-serial-list))))

(defun make-qualified-name (commodity commodity-annotation)
  (declare (type commodity commodity))
  (declare (type commodity-annotation commodity-annotation))

  (if (and (commodity-annotation-price commodity-annotation)
	   (< (amount-sign (commodity-annotation-price
			    commodity-annotation)) 0))
      (error 'amount-error :msg "A commodity's price may not be negative"))

  (with-output-to-string (out)
    (princ (commodity-symbol-name
	    (commodity-symbol commodity)) out)
    (format-commodity-annotation commodity-annotation
				 :output-stream out)))

(defun create-annotated-commodity (commodity details qualified-name)
  "Create an ANNOTATED-COMMODITY which annotates COMMODITY.
  The NAME can be either a string or a COMMODITY-SYMBOL."
  (declare (type commodity commodity))
  (declare (type commodity-annotation details))
  (declare (type string qualified-name))
  (let ((annotated-commodity
	 (make-instance 'annotated-commodity
			:referent commodity
			:details details
			:qualified-name qualified-name))
	(pool (parent-pool commodity)))

    (let ((commodities-by-serial-list
	   (commodity-pool-commodities-by-serial-list pool)))
      (setf (commodity-serial-number commodity)
	    (1+ (caar (last commodities-by-serial-list))))
      (nconc commodities-by-serial-list
	     (list (cons (commodity-serial-number commodity)
			 annotated-commodity))))

    (let ((names-map (commodity-pool-commodities-by-name-map pool)))
      (assert (not (gethash qualified-name names-map)))
      (setf (gethash qualified-name names-map) annotated-commodity))))

(defun find-annotated-commodity (name-or-commodity details
				 &key (create-if-not-exists-p nil)
				 (pool *default-commodity-pool*))
  "Find an annotated commodity matching the commodity symbol NAME
  (which may be a STRING or a COMMODITY-SYMBOL) and given set of
  commodity DETAILS, of type COMMODITY-ANNOTATION.

  Returns two values: COMMODITY or NIL, NEWLY-CREATED-P"
  (declare (type (or string commodity-symbol commodity)
		 name-or-commodity))
  (declare (type commodity-annotation details))
  (declare (type commodity-pool pool))
  (assert details)
  (assert (not (commodity-annotation-empty-p details)))
  (let ((commodity
	 (if (typep name-or-commodity 'commodity)
	     (progn
	       (assert (not (commodity-annotated-p name-or-commodity)))
	       name-or-commodity)
	     (find-commodity name-or-commodity
			     :create-if-not-exists-p create-if-not-exists-p
			     :pool pool))))
    (if commodity
	(let* ((annotated-name (make-qualified-name commodity details))
	       (annotated-commodity
		(find-commodity annotated-name :pool pool)))
	  (if annotated-commodity
	      (progn
		(assert (commodity-annotated-p annotated-commodity))
		(assert
		 (commodity-annotation-equal
		  details (commodity-annotation annotated-commodity)))
		(values annotated-commodity nil))
	      (if create-if-not-exists-p
		  (values
		   (create-annotated-commodity commodity details
					       annotated-name)
		   t))))
	(values nil nil))))

;; Amounts are bignums with a specific attached commodity.  [TODO: Also, when
;; math is performed with them, they retain knowledge of the origins of their
;; value].

;; The `keep-base' member determines whether scalable commodities are
;; automatically converted to their most reduced form when printing.  The
;; default is true.
;; 
;; For example, Ledger supports time values specified in seconds (10s), hours
;; (5.2h) or minutes.  Internally, such amounts are always kept as quantities
;; of seconds.  However, when streaming the amount Ledger will convert it to
;; its \"least representation\", which is \"5.2h\" in the second case.  If
;; `keep_base' is true, this amount is displayed as \"18720s\".

;; The following three members determine whether lot details are maintained
;; when working with commoditized values.  The default is false for all three.
;;
;; Let's say a user adds two values of the following form:
;;   10 AAPL + 10 AAPL {$20}
;;
;; This expression adds ten shares of Apple stock with another ten shares that
;; were purchased for $20 a share.  If `keep_price' is false, the result of
;; this expression will be an amount equal to 20 AAPL.  If `keep_price' is
;; true, the expression yields an exception for adding amounts with different
;; commodities.  In that case, a balance_t object must be used to store the
;; combined sum.

;; The `stream_fullstrings' static member is currently only used by
;; the unit testing code.  It causes amounts written to streams to
;; use the `to_fullstring' method rather than the `to_string'
;; method, so that complete precision is always displayed, no matter
;; what the precision of an individual commodity might be.
;; @see to_string
;; @see to_fullstring

(defun float-to-amount (value)
  (parse-amount* (format nil "~F" value)))

(defun integer-to-amount (value)
  (make-instance 'amount :quantity value :precision 0))

(defun exact-amount (in &key (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (let ((amount
	 (read-amount in :observe-properties-p nil
			 :reduce-to-smallest-units-p reduce-to-smallest-units-p
			 :pool pool)))
    (setf (slot-value amount 'keep-precision) t)
    amount))

(defun copy-amount (amount)
  (let ((tmp (make-instance 'amount :commodity (amount-commodity amount))))
    (assert (slot-boundp amount 'quantity))
    (setf (slot-value tmp 'quantity)
	  (slot-value amount 'quantity))
    (assert (slot-boundp amount 'internal-precision))
    (setf (slot-value tmp 'internal-precision)
	  (slot-value amount 'internal-precision))
    (assert (slot-boundp amount 'keep-precision))
    (setf (slot-value tmp 'keep-precision)
	  (slot-value amount 'keep-precision))
    tmp))

(defmethod copy-value ((amount amount))
  (copy-amount amount))

(defun amount-commodity-name (amount)
  (let ((commodity (amount-commodity amount)))
    (if (and commodity (commodity-annotated-p commodity))
	(setq commodity (slot-value commodity 'referent-commodity)))
    (if commodity
	(if (slot-boundp commodity 'qualified-name)
	    (slot-value commodity 'qualified-name)
	    (let ((symbol (slot-value (slot-value commodity 'basic-commodity)
				      'symbol)))
	      (if symbol
		  (progn
		    (assert (not (commodity-symbol-needs-quoting-p symbol)))
		    (commodity-symbol-name symbol))))))))

(define-condition amount-error (error) 
  ((description :reader error-description :initarg :msg))
  (:report (lambda (condition stream)
	     (format stream "~S" (error-description condition)))))

(defun verify-amounts (left right verb capitalized-gerund preposition)
  (if (or (not (slot-boundp left 'quantity))
	  (not (slot-boundp right 'quantity)))
      (cond
	((slot-boundp left 'quantity)
	 (error 'amount-error :msg
		(format nil "Cannot ~A an amount ~A an uninitialized amount"
			verb preposition)))
	((slot-boundp right 'quantity)
	 (error 'amount-error :msg
		(format nil "Cannot ~A an uninitialized amount ~A an amount"
			verb preposition)))
	(t
	 (error 'amount-error :msg
		(format nil "Cannot ~A two uninitialized amounts" verb)))))

  (unless (commodity-equal (amount-commodity left)
			   (amount-commodity right))
    (error 'amount-error :msg
	   (format nil "~A amounts with different commodities: ~A != ~A"
		   capitalized-gerund
		   (amount-commodity-name left)
		   (amount-commodity-name right)))))

(defun amount-compare (left right)
  (verify-amounts left right "compare" "Comparing" "to")
  (cond ((= (slot-value left 'internal-precision)
	    (slot-value right 'internal-precision))
	 (- (slot-value left 'quantity)
	    (slot-value right 'quantity)))
	((< (slot-value left 'internal-precision)
	    (slot-value right 'internal-precision))
	 (let ((tmp (copy-amount left)))
	   (amount--resize tmp (slot-value right 'internal-precision))
	   (- (slot-value tmp 'quantity)
	      (slot-value right 'quantity))))
	(t
	 (let ((tmp (copy-amount right)))
	   (amount--resize tmp (slot-value left 'internal-precision))
	   (- (slot-value left 'quantity)
	      (slot-value tmp 'quantity))))))

(defmethod value= ((left amount) (right amount))
  (zerop (amount-compare left right)))

(defmethod value/= ((left amount) (right amount))
  (not (zerop (amount-compare left right))))

(defmethod value-equal ((left amount) (right amount))
  (value= left right))

(defmethod value< ((left amount) (right amount))
  (minusp (amount-compare left right)))

(defmethod value<= ((left amount) (right amount))
  (let ((result (amount-compare left right)))
    (or (minusp result) (zerop result))))

(defmethod value> ((left amount) (right amount))
  (plusp (amount-compare left right)))

(defmethod value>= ((left amount) (right amount))
  (let ((result (amount-compare left right)))
    (or (plusp result) (zerop result))))

(defmethod add ((left amount) (right amount))
  (let ((tmp (copy-amount left)))
    (add* tmp right)))

(defmethod add* ((left amount) (right amount))
  (verify-amounts left right "add" "Adding" "to")
  (let ((left-quantity (slot-value left 'quantity))
	(right-quantity (slot-value right 'quantity)))
    (cond ((= (slot-value left 'internal-precision)
	      (slot-value right 'internal-precision))
	   (setf (slot-value left 'quantity)
		 (+ left-quantity right-quantity)))
	  ((< (slot-value left 'internal-precision)
	      (slot-value right 'internal-precision))
	   (amount--resize left (slot-value right 'internal-precision))
	   (setf (slot-value left 'quantity)
		 (+ left-quantity right-quantity)))
	  (t
	   (let ((tmp (copy-amount right)))
	     (amount--resize tmp (slot-value left 'internal-precision))
	     (setf (slot-value left 'quantity)
		   (+ left-quantity right-quantity))))))
  left)

(defmethod subtract ((left amount) (right amount))
  (let ((tmp (copy-amount left)))
    (subtract* tmp right)))

(defmethod subtract* ((left amount) (right amount))
  (verify-amounts left right "subtract" "Subtracting" "from")
  (let ((left-quantity (slot-value left 'quantity))
	(right-quantity (slot-value right 'quantity)))
    (cond ((= (slot-value left 'internal-precision)
	      (slot-value right 'internal-precision))
	   (setf (slot-value left 'quantity)
		 (- left-quantity right-quantity)))
	  ((< (slot-value left 'internal-precision)
	      (slot-value right 'internal-precision))
	   (amount--resize left (slot-value right 'internal-precision))
	   (setf (slot-value left 'quantity)
		 (- left-quantity right-quantity)))
	  (t
	   (let ((tmp (copy-amount right)))
	     (amount--resize tmp (slot-value left 'internal-precision))
	     (setf (slot-value left 'quantity)
		   (- left-quantity right-quantity)))))))

(defmethod multiply ((left amount) (right amount))
  (let ((tmp (copy-amount left)))
    (multiply* tmp right)))

(defun set-amount-commodity-and-round* (left right)
  (declare (type amount left))
  (declare (type amount right))

  (let ((commodity (amount-commodity left)))
    (unless commodity
      (setf (slot-value left 'commodity)
	    (setq commodity (amount-commodity right))))

    ;; If this amount has a commodity, and we're not dealing with plain
    ;; numbers, or internal numbers (which keep full precision at all
    ;; times), then round the number to within the commodity's precision
    ;; plus six places.
    (when (and commodity (not (slot-value left 'keep-precision)))
      (let ((commodity-precision (display-precision commodity)))
	(when (> (slot-value left 'internal-precision)
		 (+ 6 commodity-precision))
	  (setf (slot-value left 'quantity)
		(round (slot-value left 'quantity)
		       (expt 10 (- (slot-value left 'internal-precision)
				   (+ 6 commodity-precision)))))
	  (setf (slot-value left 'internal-precision)
		(+ 6 commodity-precision))))))
  left)

(defmethod multiply* ((left amount) (right amount))
  (verify-amounts left right "multiply" "Multiplying" "by")

  (setf (slot-value left 'quantity)
	(* (slot-value left 'quantity)
	   (slot-value right 'quantity)))
  (setf (slot-value left 'internal-precision)
	(+ (slot-value left 'internal-precision)
	   (slot-value right 'internal-precision)))

  (set-amount-commodity-and-round* left right))

(defmethod divide ((left amount) (right amount))
  (let ((tmp (copy-amount left)))
    (divide* tmp right)))

(defmethod divide ((left amount) (right integer))
  (let ((tmp (copy-amount left)))
    (divide* tmp (integer-to-amount right))))

(defmethod divide ((left integer) (right amount))
  (let ((tmp (integer-to-amount left)))
    (divide* tmp right)))

(defmethod divide* ((left amount) (right amount))
  (verify-amounts left right "divide" "Dividing" "by")

  ;; Increase the value's precision, to capture fractional parts after
  ;; the divide.  Round up in the last position.
  (setf (slot-value left 'quantity)
	(round (* (slot-value left 'quantity)
		  (expt 10 (+ 7 (* 2 (slot-value right 'internal-precision))
			      (slot-value left 'internal-precision))))
	       (slot-value right 'quantity)))
  (setf (slot-value left 'internal-precision)
	(+ 6 (* 2 (slot-value left 'internal-precision))
	   (slot-value right 'internal-precision)))

  (setf (slot-value left 'quantity)
	(round (slot-value left 'quantity)
	       (expt 10 (- (1+ (slot-value left 'internal-precision))
			   (slot-value left 'internal-precision)))))

  (set-amount-commodity-and-round* left right))

(defmethod negate* ((amount amount))
  (assert amount)
  (setf (slot-value amount 'quantity)
	(- (slot-value amount 'quantity)))
  amount)

(defmethod negate ((amount amount))
  (assert amount)
  (let ((tmp (copy-amount amount)))
    (negate* tmp)))

(defmethod absolute ((amount amount))
  (assert amount)
  (if (minusp (slot-value amount 'quantity))
      (negate amount)
      amount))

(defmethod round-to-precision* ((amount amount) &optional precision)
  "Round the given AMOUNT to the stated internal PRECISION.
  If PRECISION is less than the current internal precision, data will
  be lost.  If it is greater, the integer value of the amount is
  increased until the target precision is reached."
  (unless precision
    (let ((commodity (amount-commodity amount)))
      (if commodity
	  (setq precision (display-precision commodity))
	  (setq precision 0))))

  (let ((internal-precision (slot-value amount 'internal-precision)))
    (cond ((< precision internal-precision)
	   (setf (slot-value amount 'quantity)
		 (nth-value 0 (truncate (slot-value amount 'quantity)
					(expt 10 (- internal-precision
						    precision)))))
	   (setf (slot-value amount 'internal-precision) precision))
	  ((> precision internal-precision)
	   (setf (slot-value amount 'quantity)
		 (* (slot-value amount 'quantity)
		    (expt 10 (- precision internal-precision))))
	   (setf (slot-value amount 'internal-precision) precision))))
  amount)

(defmethod round-to-precision ((amount amount) &optional precision)
  (let ((tmp (copy-amount amount)))
    (round-to-precision* tmp precision)))

(defmethod unround ((amount amount))
  (assert amount)
  (if (slot-boundp amount 'quantity)
      (if (slot-value amount 'keep-precision)
	  amount)
      (error 'amount-error :msg "Cannot unround an uninitialized amount"))
  (let ((tmp (copy-amount amount)))
    (setf (slot-value tmp 'keep-precision) t)
    tmp))

(defmethod smallest-units* ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot reduce an uninitialized amount");
  ;;
  ;; while (commodity_ && commodity().smaller()) {
  ;;   *this *= commodity().smaller()->number();
  ;;   commodity_ = commodity().smaller()->commodity_;
  ;; }
  ;; return *this;
  (assert amount))

(defmethod smallest-units ((amount amount))
  (let ((tmp (copy-amount amount)))
    (smallest-units* tmp)
    tmp))

(defmethod larger-units* ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot unreduce an uninitialized amount");
  ;;
  ;; while (commodity_ && commodity().larger()) {
  ;;   *this /= commodity().larger()->number();
  ;;   commodity_ = commodity().larger()->commodity_;
  ;;   if (abs() < amount_t(1.0))
  ;;     break;
  ;; }
  ;; return *this;
  (assert amount))

(defmethod larger-units ((amount amount))
  (let ((tmp (copy-amount amount)))
    (larger-units* tmp)
    tmp))

(defmethod market-value ((amount amount) &optional datetime)
  ;; if (quantity) {
  (assert amount)
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg
	   "Cannot determine the market value of an uninitialized amount"))

  (let ((commodity (amount-commodity amount)))
    (when commodity
      (let ((value (market-value commodity datetime)))
	(if value
	    (round-to-precision* (multiply* value amount)))))))

(defun amount-sign (amount)
  "Return -1, 0 or 1 depending on the sign of AMOUNT."
  (assert amount)
  (let ((quantity (slot-value amount 'quantity)))
    (if (minusp quantity)
	-1
	(if (plusp quantity)
	    1
	    0))))

(defmethod zero-p ((amount amount))
  (assert amount)
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg
	   "Cannot determine whether an uninitialized amount is zero"))
  (let ((commodity (amount-commodity amount)))
    (if commodity
	(if (<= (slot-value amount 'internal-precision)
		(display-precision commodity))
	    (real-zero-p amount)
	    (= 0 (amount-sign (round-to-precision
			       (display-precision commodity)))))
	(real-zero-p amount))))

(defmethod real-zero-p ((amount amount))
  (assert amount)
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg
	   "Cannot determine whether an uninitialized amount is zero"))
  (= 0 (slot-value amount 'quantity)))

(defun amount-to-integer (amount &key (dont-check-p t))
  (declare (type amount amount))
  (declare (type boolean dont-check-p))
  (multiple-value-bind (quotient remainder)
      (truncate (slot-value amount 'quantity)
		(expt 10 (slot-value amount 'internal-precision)))
    (if (and (not dont-check-p) remainder)
	(error 'amount-error :msg
	       "Conversion of amount to_long loses precision"))
    quotient))

(defmethod format-value ((amount amount))
  (assert amount)
  (with-output-to-string (out)
    (print-value amount :output-stream out)
    out))

(defmethod convert-to-fullstring ((amount amount))
  (assert amount)
  (with-output-to-string (out)
    (print-value amount :output-stream out :full-precision-p t)
    out))

(defun quantity-string (amount)
  (assert amount)
  (with-output-to-string (out)
    (print-value amount :output-stream out :omit-commodity-p t)
    out))

(defun amount-lessp (left right)
  (minusp (amount-compare left right)))

(defun amount-greaterp (left right)
  (plusp (amount-compare left right)))

(defun amount-quantity (amount)
  (declare (type amount amount))
  (let ((commodity (amount-commodity amount)))
    (if commodity
	(let ((tmp (copy-amount amount)))
	  (setf (slot-value tmp 'commodity) nil)
	  tmp)
	amount)))

(defmethod annotate-commodity ((amount amount)
			       (details commodity-annotation))
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg
	   "Cannot annotate the commodity of an uninitialized amount"))

  (let ((commodity (amount-commodity amount)))
    (unless commodity
      (error 'amount-error :msg
	     "Cannot annotate an amount which has no commodity"))
    (let ((referent commodity))
      (if (commodity-annotated-p referent)
	  (setq referent (slot-value referent 'referent-commodity)))

      (let ((annotated-commodity
	     (find-annotated-commodity referent details
				       :create-if-not-exists-p t)))
	(let ((tmp (copy-amount amount)))
	  (setf (amount-commodity tmp) annotated-commodity)
	  tmp)))))

(defmethod commodity-annotated-p ((commodity commodity))
  (slot-value commodity 'annotated-p))

(defmethod commodity-annotated-p ((annotated-commodity annotated-commodity))
  t)

(defmethod commodity-annotated-p ((amount amount))
  (commodity-annotated-p (amount-commodity amount)))

(defmethod commodity-annotation ((annotated-commodity annotated-commodity))
  (slot-value annotated-commodity 'annotation))

(defmethod commodity-annotation ((amount amount))
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg
	   "Cannot return annotation details for an uninitialized amount"))
  ;; This calls the appropriate generic function
  (commodity-annotation (amount-commodity amount)))

(defun read-amount-quantity (in)
  (declare (type stream in))
  (let ((buf (make-string-output-stream))
	last-special)
    (do ((c (read-char in) (read-char in nil 'the-end)))
	((not (characterp c)))
      (if (digit-char-p c)
	  (progn
	    (when last-special
	      (write-char last-special buf)
	      (setq last-special nil))
	    (write-char c buf))
	  (if (and (null last-special)
		   (or (char= c #\-)
		       (char= c #\.)
		       (char= c #\,)))
	      (setq last-special c)
	      (progn
		(unread-char c in)
		(return)))))
    (if last-special
	(unread-char last-special in))
    (get-output-stream-string buf)))

(defun peek-char-in-line (in &optional skip-whitespace)
  (declare (type stream in))
  (do ((c (peek-char nil in nil 'the-end)
	  (peek-char nil in nil 'the-end)))
      ((or (not (characterp c))
	   (char= #\Newline c)))
    (if (and skip-whitespace
	     (or (char= #\Space c)
		 (char= #\Tab c)))
	(read-char in)
	(return c))))

(defun read-amount (in &key (observe-properties-p t)
		    (reduce-to-smallest-units-p t)
		    (pool *default-commodity-pool*))
  "Parse an AMOUNT from the input IN, which may be a stream or string.

  If :OBSERVE-PROPERTIES-P is T (the default), any display details noticed in
  this amount will be set as defaults for displaying this kind of commodity in
  the future.

  If :REDUCE-TO-SMALLEST-UNITS is T (the default), the resulting value will be
  expressed in terms of its finest units -- for example, 1h might be returned
  as 60m or 3600s, depending on what other units have been defined.

  If :POOL is set, any commodities created by this routine (a maximum possible
  of two, if an annotated price is given with a second commodity) will be
  associated with the given commodity pool.

  The possible syntax for an amount is:
  
  [-]NUM[ ]SYM [ANNOTATION]
  SYM[ ][-]NUM [ANNOTATION]"
  (declare (type (or stream string null) in))

  (let ((in (get-input-stream in))
	symbol quantity details negative-p
	(connected-p t) (prefixed-p t) thousand-marks-p
	amount)

    (when (char= #\- (peek-char-in-line in t))
      (setq negative-p t)
      (read-char in))

    (if (digit-char-p (peek-char-in-line in t))
	(progn
	  (setq quantity (read-amount-quantity in))
	  (assert quantity)

	  (let ((c (peek-char-in-line in)))
	    (if (and (characterp c)
		     (char= #\Space c))
		(setq connected-p nil))
	    (let ((n (peek-char-in-line in t)))
	      (when (and (characterp n)
			 (not (char= #\Newline n)))
		(setq symbol (read-commodity-symbol in))
		(if symbol
		    (setq prefixed-p nil))))))
	(progn
	  (setq symbol (read-commodity-symbol in))
	  (if (char= #\Space (peek-char nil in))
	      (setq connected-p nil))
	  (let ((n (peek-char-in-line in t)))
	    (if (and (characterp n)
		     (not (char= #\Newline n)))
		(setq quantity (read-amount-quantity in))
		(error 'amount-error :msg
		       "No quantity specified for amount")))))

    (let ((c (peek-char-in-line in t)))
      (if (and (characterp c)
	       (or (char= c #\{)
		   (char= c #\[)
		   (char= c #\()))
	  (setq details (read-commodity-annotation in))))

    ;; Now that we have the full commodity symbol, create the commodity object
    ;; it refers to
    (multiple-value-bind (commodity newly-created-p)
	(if (and symbol
		 (not (zerop (length (commodity-symbol-name symbol)))))
	    (if details
		(find-annotated-commodity symbol details :pool pool
					  :create-if-not-exists-p t)
		(find-commodity symbol :pool pool :create-if-not-exists-p t))
	    (values nil nil))

      ;; Determine the precision of the amount, based on the usage of
      ;; comma or period.
      (setq amount (make-instance 'amount :commodity commodity))

      (let ((last-comma (position #\, quantity :from-end t))
	    (last-period (position #\. quantity :from-end t)))
	(if (or (and *european-style* last-period)
		(and (not *european-style*) last-comma))
	    (setq thousand-marks-p t))
	(cond ((and last-comma last-period)
	       (setf (slot-value amount 'internal-precision)
		     (- (length quantity) (if (> last-comma last-period)
					      last-comma last-period) 1)))
	      ((and last-comma *european-style*)
	       (setf (slot-value amount 'internal-precision)
		     (- (length quantity) last-comma 1)))
	      ((and last-period (not *european-style*))
	       (setf (slot-value amount 'internal-precision)
		     (- (length quantity) last-period 1)))
	      (t
	       (setf (slot-value amount 'internal-precision) 0))))

      (setf (slot-value amount 'quantity)
	    (parse-integer (delete-if #'(lambda (c)
					  (or (char= #\. c)
					      (char= #\, c))) quantity)))

      (when (and commodity (or newly-created-p observe-properties-p))
	;; Observe the commodity usage details we noticed while parsing
	(setf (commodity-symbol-prefixed-p
	       (commodity-symbol commodity)) prefixed-p)
	(setf (commodity-symbol-connected-p
	       (commodity-symbol commodity)) connected-p)
	(if thousand-marks-p
	    (setf (slot-value (commodity-base commodity) 'thousand-marks-p)
		  thousand-marks-p))

	(if (> (slot-value amount 'internal-precision)
	       (display-precision commodity))
	    (setf (slot-value (commodity-base commodity) 'display-precision)
		  (slot-value amount 'internal-precision)))))

    (if negative-p
	(negate* amount))

    (if reduce-to-smallest-units-p
	(smallest-units* amount))

    amount))

(defun read-amount* (in &key (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (read-amount in :observe-properties-p nil
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun amount (in &key (reduce-to-smallest-units-p t)
	       (pool *default-commodity-pool*))
  (read-amount in :observe-properties-p t
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun amount* (in &key (reduce-to-smallest-units-p t)
	       (pool *default-commodity-pool*))
  (read-amount in :observe-properties-p nil
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun parse-amount (in &key (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (read-amount in :observe-properties-p t
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun parse-amount* (in &key (reduce-to-smallest-units-p t)
		      (pool *default-commodity-pool*))
  (read-amount in :observe-properties-p nil
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun parse-amount-conversion (larger-string smaller-string)
  ;; amount_t larger, smaller;
  ;;
  ;; larger.parse(larger_str, AMOUNT_PARSE_NO_REDUCE);
  ;; smaller.parse(smaller_str, AMOUNT_PARSE_NO_REDUCE);
  ;;
  ;; larger *= smaller.number();
  ;;
  ;; if (larger.commodity()) {
  ;;   larger.commodity().set_smaller(smaller);
  ;;   larger.commodity().add_flags(smaller.commodity().flags() |
  ;;                                COMMODITY_STYLE_NOMARKET);
  ;; }
  ;; if (smaller.commodity())
  ;;   smaller.commodity().set_larger(larger);
  (assert larger-string)
  (assert smaller-string))

;; jww (2007-10-15): use keywords here
(defmethod print-value ((amount amount) &key
			(output-stream *standard-output*)
			(omit-commodity-p nil)
			(full-precision-p nil))
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg "Cannot print an uninitialized amount"))

  ;; jww (2007-10-17): This should change from a simple boolean to registered
  ;; commodity to which values should be converted (possibly in both
  ;; directions)
  (unless (slot-value amount 'keep-base)
    ;; amount_t base(*this);
    ;; if (! amount_t::keep_base)
    ;;   base.in_place_unreduce();
    )

  (let* ((commodity (amount-commodity amount))
	 (omit-commodity-p (or omit-commodity-p (null commodity)))
	 (commodity-symbol (and (not omit-commodity-p) commodity
				(commodity-symbol commodity)))
	 (precision (slot-value amount 'internal-precision))
	 (display-precision
	  (if (or (null commodity)
		  full-precision-p
		  (slot-value amount 'keep-precision))
	      (slot-value amount 'internal-precision)
	      (display-precision amount))))

    (assert (or (null commodity-symbol)
		(> (length (commodity-symbol-name commodity-symbol)) 0)))

    (multiple-value-bind (quotient remainder)
	(truncate (slot-value amount 'quantity)
		  (expt 10 precision))
      (cond ((< display-precision precision)
	     (setq remainder
		   (nth-value 0 (truncate remainder
					  (expt 10 (- precision
						      display-precision))))))
	    ((> display-precision precision)
	     (setq remainder (* remainder
				(expt 10 (- display-precision
					    precision))))))
      (flet ((maybe-gap ()
	       (unless (commodity-symbol-connected-p commodity-symbol)
		 (princ #\Space output-stream))))
	(when (and (not omit-commodity-p)
		   (commodity-symbol-prefixed-p commodity-symbol))
	  (princ (amount-commodity-name amount) output-stream)
	  (maybe-gap))

	(format output-stream "~:[~,,vD~;~,,v:D~]" ;
		(and commodity
		     (commodity-thousand-marks-p commodity))
		(if *european-style* #\. #\,)
		quotient)
	(unless (zerop display-precision)
	  (format output-stream "~C~v,'0D"
		  (if *european-style* #\, #\.)
		  display-precision remainder))
      
	(when (and (not omit-commodity-p)
		   (not (commodity-symbol-prefixed-p commodity-symbol)))
	  (maybe-gap)
	  (princ (amount-commodity-name amount) output-stream)))

      (if (and (not omit-commodity-p)
	       commodity
	       (commodity-annotated-p commodity))
	  (format-commodity-annotation (commodity-annotation commodity)
				       :output-stream output-stream)))))

;; jww (2007-10-15): Add back this builtin commodity
;;  commodity->add_flags(COMMODITY_STYLE_NOMARKET | COMMODITY_STYLE_BUILTIN);
;;
;;  parse_conversion("1.0m", "60s");
;;  parse_conversion("1.0h", "60m");

(defun amount--resize (amount precision)
  (assert (< precision 256))
  (unless (or (not (slot-boundp amount 'quantity))
	      (= precision (slot-value amount 'internal-precision)))
    (assert (> precision (slot-value amount 'internal-precision)))
    (setf (slot-value amount 'quantity)
	  (* (slot-value amount 'quantity)
	     (expt 10 (- precision
			 (slot-value amount 'internal-precision)))))
    (setf (slot-value amount 'internal-precision) precision)))

(defmethod add-to-balance ((balance balance) (amount amount))
  ;; TRACE_CTOR(balance_t, "const amount_t&");
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot initialize a balance from an uninitialized amount");
  ;; if (! amt.is_realzero())
  ;;   amounts.insert(amounts_map::value_type(&amt.commodity(), amt));
  (assert (or balance amount)))

;; jww (2007-10-15): What's the difference between FLOAT and REAL?
(defmethod add-to-balance ((balance balance) (real real))
  ;; TRACE_CTOR(balance_t, "const double");
  ;; amounts.insert
  ;;   (amounts_map::value_type(amount_t::current_pool->null_commodity, val));
  (assert (or balance real)))

(defmethod add-to-balance ((balance balance) (integer integer))
  ;; TRACE_CTOR(balance_t, "const unsigned long");
  ;; amounts.insert
  ;;   (amounts_map::value_type(amount_t::current_pool->null_commodity, val));
  (assert (or balance integer)))

(defun balance-set-to-amount (amount)
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot assign an uninitialized amount to a balance");
  ;;
  ;; amounts.clear();
  ;; if (! amt.is_realzero())
  ;;   amounts.insert(amounts_map::value_type(&amt.commodity(), amt));
  ;;
  ;; return *this;
  (assert amount))

(defun balances-equal (balance)
  ;; amounts_map::const_iterator i, j;
  ;; for (i = amounts.begin(), j = bal.amounts.begin();
  ;;      i != amounts.end() && j != bal.amounts.end();
  ;;      i++, j++) {
  ;;   if (! (i->first == j->first && i->second == j->second))
  ;;     return false;
  ;; }
  ;; return i == amounts.end() && j == bal.amounts.end();
  (assert balance))

(defun balance-equal-to-amount (amount)
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot compare a balance to an uninitialized amount");
  ;;
  ;; if (amt.is_realzero())
  ;;   return amounts.empty();
  ;; else
  ;;   return amounts.size() == 1 && amounts.begin()->second == amt;
  (assert amount))

;; Binary arithmetic operators.  Balances support addition and
;; subtraction of other balances or amounts, but multiplication and
;; division are restricted to uncommoditized amounts only.
(defmethod add* ((balance balance) (other balance))
  ;; for (amounts_map::const_iterator i = bal.amounts.begin();
  ;;      i != bal.amounts.end();
  ;;      i++)
  ;;   *this += i->second;
  ;; return *this;
  (assert (or balance other)))

(defmethod add* ((balance balance) (other amount))
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot add an uninitialized amount to a balance");
  ;;
  ;; if (amt.is_realzero())
  ;;   return *this;
  ;;
  ;; amounts_map::iterator i = amounts.find(&amt.commodity());
  ;; if (i != amounts.end())
  ;;   i->second += amt;
  ;; else
  ;;   amounts.insert(amounts_map::value_type(&amt.commodity(), amt));
  ;;
  ;; return *this;
  (assert (or balance other)))

(defmethod subtract* ((balance balance) (other balance))
  ;; for (amounts_map::const_iterator i = bal.amounts.begin();
  ;;      i != bal.amounts.end();
  ;;      i++)
  ;;   *this -= i->second;
  ;; return *this;
  (assert (or balance other)))

(defmethod subtract* ((balance balance) (other amount))
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot subtract an uninitialized amount from a balance");
  ;;
  ;; if (amt.is_realzero())
  ;;   return *this;
  ;;
  ;; amounts_map::iterator i = amounts.find(&amt.commodity());
  ;; if (i != amounts.end()) {
  ;;   i->second -= amt;
  ;;   if (i->second.is_realzero())
  ;;     amounts.erase(i);
  ;; } else {
  ;;   amounts.insert(amounts_map::value_type(&amt.commodity(), amt.negate()));
  ;; }
  ;; return *this;
  (assert (or balance other)))

(defmethod multiply* ((balance balance) (other amount))
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot multiply a balance by an uninitialized amount");
  ;;
  ;; if (is_realzero()) {
  ;;   ;
  ;; }
  ;; else if (amt.is_realzero()) {
  ;;   *this = amt;
  ;; }
  ;; else if (! amt.commodity()) {
  ;;   // Multiplying by an amount with no commodity causes all the
  ;;   // component amounts to be increased by the same factor.
  ;;   for (amounts_map::iterator i = amounts.begin();
  ;;        i != amounts.end();
  ;;        i++)
  ;;     i->second *= amt;
  ;; }
  ;; else if (amounts.size() == 1) {
  ;;   // Multiplying by a commoditized amount is only valid if the sole
  ;;   // commodity in the balance is of the same kind as the amount's
  ;;   // commodity.
  ;;   if (*amounts.begin()->first == amt.commodity())
  ;;     amounts.begin()->second *= amt;
  ;;   else
  ;;     throw_(balance_error,
  ;;            "Cannot multiply a balance with annotated commodities by a commoditized amount");
  ;; }
  ;; else {
  ;;   assert(amounts.size() > 1);
  ;;   throw_(balance_error,
  ;;          "Cannot multiply a multi-commodity balance by a commoditized amount");
  ;; }
  ;; return *this;
  (assert (or balance other)))

(defmethod divide* ((balance balance) (other amount))
  ;; if (amt.is_null())
  ;;   throw_(balance_error,
  ;;          "Cannot divide a balance by an uninitialized amount");
  ;;
  ;; if (is_realzero()) {
  ;;   ;
  ;; }
  ;; else if (amt.is_realzero()) {
  ;;   throw_(balance_error, "Divide by zero");
  ;; }
  ;; else if (! amt.commodity()) {
  ;;   // Dividing by an amount with no commodity causes all the
  ;;   // component amounts to be divided by the same factor.
  ;;   for (amounts_map::iterator i = amounts.begin();
  ;;        i != amounts.end();
  ;;        i++)
  ;;     i->second /= amt;
  ;; }
  ;; else if (amounts.size() == 1) {
  ;;   // Dividing by a commoditized amount is only valid if the sole
  ;;   // commodity in the balance is of the same kind as the amount's
  ;;   // commodity.
  ;;   if (*amounts.begin()->first == amt.commodity())
  ;;     amounts.begin()->second /= amt;
  ;;   else
  ;;     throw_(balance_error,
  ;;            "Cannot divide a balance with annotated commodities by a commoditized amount");
  ;; }
  ;; else {
  ;;   assert(amounts.size() > 1);
  ;;   throw_(balance_error,
  ;;          "Cannot divide a multi-commodity balance by a commoditized amount");
  ;; }
  ;; return *this;
  (assert (or balance other)))

(defun copy-balance (balance)
  (assert balance))

(defmethod negate* ((balance balance))
  ;; for (amounts_map::iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   i->second.in_place_negate();
  ;; return *this;
  (assert balance))

(defmethod negate ((balance balance))
  (let ((tmp (copy-balance balance)))
    ;; (negate* tmp)
    (assert tmp)
    ))

(defmethod absolute ((balance balance))
  ;; balance_t temp;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   temp += i->second.abs();
  ;; return temp;
  (assert balance))

(defmethod smallest-units ((balance balance))
  ;; balance_t temp(*this);
  ;; temp.in_place_reduce();
  ;; return temp;
  (assert balance))

(defmethod smallest-units* ((balance balance))
  ;; // A temporary must be used here because reduction may cause
  ;; // multiple component amounts to collapse to the same commodity.
  ;; balance_t temp;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   temp += i->second.reduce();
  ;; return *this = temp;
  (assert balance))

(defmethod larger-units ((balance balance))
  ;; balance_t temp(*this);
  ;; temp.in_place_unreduce();
  ;; return temp;
  (assert balance))

(defmethod larger-units* ((balance balance))
  ;; // A temporary must be used here because unreduction may cause
  ;; // multiple component amounts to collapse to the same commodity.
  ;; balance_t temp;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   temp += i->second.unreduce();
  ;; return *this = temp;
  (assert balance))

(defmethod market-value ((balance balance) &optional datetime)
  ;; optional<balance_t> temp;
  ;;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   if (optional<amount_t> val = i->second.value(moment)) {
  ;;     if (! temp)
  ;;       temp = balance_t();
  ;;     *temp += *val;
  ;;   }
  ;;
  ;; return temp;
  (assert balance)
  (assert datetime))

(defmethod zero-p ((balance balance))
  ;; if (is_empty())
  ;;   return true;
  ;;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   if (! i->second.is_zero())
  ;;     return false;
  ;; return true;
  (assert balance))

(defmethod convert-to-amount ((balance balance))
  ;; if (is_empty())
  ;;   throw_(balance_error, "Cannot convert an empty balance to an amount");
  ;; else if (amounts.size() == 1)
  ;;   return amounts.begin()->second;
  ;; else
  ;;   throw_(balance_error,
  ;;          "Cannot convert a balance with multiple commodities to an amount");
  (assert balance))

(defmethod amount-in-balance ((balance balance) (commodity commodity))
  ;; // jww (2007-05-20): Needs work
  ;; if (! commodity) {
  ;;   if (amounts.size() == 1) {
  ;;     amounts_map::const_iterator i = amounts.begin();
  ;;     return i->second;
  ;;   }
  ;;   else if (amounts.size() > 1) {
  ;;     // Try stripping annotations before giving an error.
  ;;     balance_t temp(strip_annotations());
  ;;     if (temp.amounts.size() == 1)
  ;;       return temp.commodity_amount(commodity);
  ;;
  ;;     throw_(amount_error,
  ;;            "Requested amount of a balance with multiple commodities: " << temp);
  ;;   }
  ;; }
  ;; else if (amounts.size() > 0) {
  ;;   amounts_map::const_iterator i = amounts.find(&*commodity);
  ;;   if (i != amounts.end())
  ;;     return i->second;
  ;; }
  ;; return none;
  (assert balance)
  (assert commodity))

(defmethod strip-annotations ((balance balance)
			      &key keep-price keep-date keep-tag)
  ;; balance_t temp;
  ;;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   temp += i->second.strip_annotations(keep_price, keep_date, keep_tag);
  ;;
  ;; return temp;
  (assert balance)
  (assert (or keep-price keep-date keep-tag)))

(defun print-balance (balance &optional out first-width latter-width)
  "Printing methods.  A balance may be output to a stream using the `print'
   method.  There is also a global operator<< defined which simply calls print
   for a balance on the given stream.  There is one form of the print method,
   which takes two required arguments and one arguments with a default value:
   
   print(ostream, int first_width, int latter_width) prints a balance to the
   given output stream, using each commodity's default display
   characteristics.  The first_width parameter specifies the width that should
   be used for printing amounts (since they are likely to vary in width).  The
   latter_width, if specified, gives the width to be used for each line after
   the first.  This is useful when printing in a column which falls at the
   right-hand side of the screen.
   
   In addition to the width constraints, balances will also print with
   commodities in alphabetized order, regardless of the relative amounts of
   those commodities.  There is no option to change this behavior."
  ;; bool first  = true;
  ;; int  lwidth = latter_width;
  ;;
  ;; if (lwidth == -1)
  ;;   lwidth = first_width;
  ;;
  ;; typedef std::vector<const amount_t *> amounts_array;
  ;; amounts_array sorted;
  ;;
  ;; for (amounts_map::const_iterator i = amounts.begin();
  ;;      i != amounts.end();
  ;;      i++)
  ;;   if (i->second)
  ;;     sorted.push_back(&i->second);
  ;;
  ;; std::stable_sort(sorted.begin(), sorted.end(),
  ;;                  compare_amount_commodities());
  ;;
  ;; for (amounts_array::const_iterator i = sorted.begin();
  ;;      i != sorted.end();
  ;;      i++) {
  ;;   int width;
  ;;   if (! first) {
  ;;     out << std::endl;
  ;;     width = lwidth;
  ;;   } else {
  ;;     first = false;
  ;;     width = first_width;
  ;;   }
  ;;
  ;;   out.width(width);
  ;;   out.fill(' ');
  ;;   out << std::right << **i;
  ;; }
  ;;
  ;; if (first) {
  ;;   out.width(first_width);
  ;;   out.fill(' ');
  ;;   out << std::right << "0";
  ;; }
  (assert (or balance out first-width latter-width)))

;; cambl.lisp ends here
