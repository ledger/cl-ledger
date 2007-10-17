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

(declaim (optimize (safety 3) (debug 3)))

(defpackage :cambl
  (:use :common-lisp)
  (:export :make-commodity-pool
	   :*default-commodity-pool*
	   :create-commodity
	   :commodity-error
	   :amount
	   :amount-error
	   :amount-commodity
	   :copy-amount
	   :copy-value
	   :float-to-amount
	   :integer-to-amount
	   :exact-amount
	   :parse-amount
	   :parse-amount-lightly
	   :read-amount
	   :read-amount-lightly
	   :print-value
	   :format-value
	   :display-precision
	   :value=
	   :value-equal
	   :value-equalp
	   :add
	   :add*
	   :subtract
	   :subtract*
	   :multiply
	   :multiply*
	   :divide
	   :divide*
	   :*european-style*
	   :*amount-stream-fullstrings*))

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

(defstruct commodity-price-history
  (prices nil :type list)
  (last-lookup nil :type (or datetime null)))

(defclass basic-commodity ()
  ((symbol :initarg :symbol :type commodity-symbol)
   (name :accessor commodity-name)
   (note :accessor commodity-note)
   (smaller-unit-equivalence :accessor smaller-unit-equivalence)
   (larger-unit-equivalence :accessor larger-unit-equivalence)
   (thousand-marks-p :initarg :thousand-marks-p :initform nil
		     :type boolean)
   (no-market-price-p :initarg :no-market-price-p :initform nil
		      :type boolean)
   (builtin-p :initarg :builtin-p :initform nil :type boolean)
   (display-precision :initform 0 :type fixnum)
   (price-history :accessor commodity-price-history
		  :type commodity-price-history)))

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
   qualified-symbol
   (has-qualified-symbol-p :type boolean :initform nil)
   mapping-key
   (annotated-p :initform nil :type boolean)))

(defclass amount ()
  ((commodity :accessor amount-commodity :initarg :commodity
	      :initform nil :type (or commodity null))
   (quantity :type integer)
   (internal-precision :type fixnum)
   (keep-precision :initform nil :type boolean)
   ;;value-origins-list
   ;; (commodity-pool :accessor amount-commodity-pool :initarg :pool
   ;;   :type commodity-pool)
   (keep-base :allocation :class :initform nil :type boolean)
   (keep-price :allocation :class :initform nil :type boolean)
   (keep-date :allocation :class :initform nil :type boolean)
   (keep-tag :allocation :class :initform nil :type boolean)))

(defvar *amount-stream-fullstrings* nil)

(defstruct commodity-annotation
  (price nil :type (or amount null))
  (date nil :type (or datetime null))
  (tag nil :type (or string null)))

(defclass annotated-commodity (commodity)
  ((referent-commodity :type commodity)
   (annotation :accessor commodity-annotation :type commodity-annotation)))

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
(defgeneric strip-annotations (commodity &optional keep-price keep-date keep-tag))
(defgeneric commodity-annotated-p (item))
(defgeneric commodity-annotation (item))
(defgeneric commodity-annotation-empty-p (annotation))
(defgeneric annotate-commodity (commodity annotation))
(defgeneric copy-value (value))
(defgeneric print-value (value &key output-stream omit-commodity-p full-precision-p))
(defgeneric format-value (value))
(defgeneric negate* (value))
(defgeneric negate (value))
(defgeneric zero-p (amount))
(defgeneric real-zero-p (amount))
(defgeneric value= (left right))
(defgeneric value-equal (left right))
(defgeneric value-equalp (left right))
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

(defmethod commodity-base ((basic-commodity basic-commodity))
  basic-commodity)

(defmethod commodity-base ((commodity commodity))
  (slot-value commodity 'basic-commodity))

(defmethod commodity-symbol ((basic-commodity basic-commodity))
  (let ((symbol (slot-value basic-commodity 'symbol)))
    (assert (not (zerop (length (commodity-symbol-name symbol)))))
    symbol))

(defmethod commodity-symbol ((commodity commodity))
  (if (slot-value commodity 'has-qualified-symbol-p)
      (slot-value commodity 'qualified-symbol)
      (commodity-symbol (slot-value commodity 'basic-commodity))))

(defmethod commodity-thousand-marks-p ((basic-commodity basic-commodity))
  (slot-value basic-commodity 'thousand-marks-p))

(defmethod commodity-thousand-marks-p ((commodity commodity))
  (commodity-thousand-marks-p (slot-value commodity 'basic-commodity)))

(defmethod display-precision ((basic-commodity basic-commodity))
  (slot-value basic-commodity 'display-precision))

(defmethod display-precision ((commodity commodity))
  (display-precision (slot-value commodity 'basic-commodity)))

(defmethod display-precision ((amount amount))
  (display-precision (amount-commodity amount)))

(defmethod commodity-equal ((a commodity) (b null))
  nil)

(defmethod commodity-equal ((a null) (b commodity))
  nil)

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

      (unless (commodity-equal left-commodity right-commodity)
	(return (string-lessp (commodity-symbol-name
			       (commodity-symbol left-commodity))
			      (commodity-symbol-name
			       (commodity-symbol right-commodity)))))

      (if (and (commodity-annotated-p left-commodity)
	       (not (commodity-annotated-p right-commodity)))
	  (return t))

      (if (and (not (commodity-annotated-p left-commodity))
	       (commodity-annotated-p right-commodity))
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

(defun add-price-point (commodity price datetime)
  ;; if (! base->history)
  ;;   base->history = history_t();
  ;;
  ;; history_map::iterator i = base->history->prices.find(date);
  ;; if (i != base->history->prices.end()) {
  ;;   (*i).second = price;
  ;; } else {
  ;;   std::pair<history_map::iterator, bool> result
  ;;     = base->history->prices.insert(history_map::value_type(date, price));
  ;;   assert(result.second);
  ;; }
  (assert commodity)
  (assert price)
  (assert datetime))

(defun remove-price-point (commodity datetime)
  ;; if (base->history) {
  ;;   history_map::size_type n = base->history->prices.erase(date);
  ;;   if (n > 0) {
  ;;     if (base->history->prices.empty())
  ;;    base->history.reset();
  ;;     return true;
  ;;   }
  ;; }
  ;; return false;
  (assert commodity)
  (assert datetime))

(defmethod market-value ((commodity commodity) &optional datetime)
  ;; optional<moment_t> age;
  ;; optional<amount_t> price;
  ;;
  ;; if (base->history) {
  ;;   assert(base->history->prices.size() > 0);
  ;;
  ;;   if (! moment) {
  ;;     history_map::reverse_iterator r = base->history->prices.rbegin();
  ;;     age   = (*r).first;
  ;;     price = (*r).second;
  ;;   } else {
  ;;     history_map::iterator i = base->history->prices.lower_bound(*moment);
  ;;     if (i == base->history->prices.end()) {
  ;;       history_map::reverse_iterator r = base->history->prices.rbegin();
  ;;       age   = (*r).first;
  ;;       price = (*r).second;
  ;;     } else {
  ;;       age = (*i).first;
  ;;       if (*moment != *age) {
  ;;         if (i != base->history->prices.begin()) {
  ;;           --i;
  ;;           age   = (*i).first;
  ;;           price = (*i).second;
  ;;         } else {
  ;;           age   = none;
  ;;         }
  ;;       } else {
  ;;         price = (*i).second;
  ;;       }
  ;;     }
  ;;   }
  ;; }
  ;;
  ;; if (! has_flags(COMMODITY_STYLE_NOMARKET) && parent().get_quote) {
  ;;   if (optional<amount_t> quote = parent().get_quote
  ;;       (*this, age, moment,
  ;;        (base->history && base->history->prices.size() > 0 ?
  ;;         (*base->history->prices.rbegin()).first : optional<moment_t>())))
  ;;     return *quote;
  ;; }
  ;; return price;
  (assert commodity)
  (assert datetime))

(defun get-price-quote (symbol &optional datetime)
  (assert symbol)
  (assert datetime)
  (format t "I don't know how to download prices yet."))

;; annotated-commodity's are references to other commodities (which in turn
;; reference a basic-commodity) which carry along additional contextual
;; information relating to a point in time.

(defmethod initialize-instance :after
    ((annotated-commodity annotated-commodity) &key)
  (setf (slot-value (slot-value annotated-commodity 'referent-commodity)
		    'annotated-p) t))

(defmethod commodity-base ((annotated-commodity annotated-commodity))
  (slot-value (slot-value annotated-commodity 'referent-commodity)
	      'basic-commodity))

;; jww (2007-10-15): use keywords here
(defmethod strip-annotations ((commodity commodity)
			      &optional keep-price keep-date keep-tag)
  ;; if (! quantity)
  ;;   throw_(amount_error,
  ;;          "Cannot strip commodity annotations from an uninitialized amount");
  ;;
  ;; if (! commodity().annotated ||
  ;;     (_keep_price && _keep_date && _keep_tag))
  ;;   return *this;
  ;;
  ;; amount_t t(*this);
  ;; t.set_commodity(as_annotated_commodity(commodity()).
  ;;                 strip_annotations(_keep_price, _keep_date, _keep_tag));
  ;; return t;
  (assert commodity)
  (assert (or keep-price keep-date keep-tag)))

(defmethod strip-annotations ((annotated-commodity annotated-commodity)
			      &optional keep-price keep-date keep-tag)
  ;; DEBUG("commodity.annotated.strip",
  ;;       "Reducing commodity " << *this << std::endl
  ;;        << "  keep price " << _keep_price << " "
  ;;        << "  keep date "  << _keep_date << " "
  ;;        << "  keep tag "   << _keep_tag);
  ;;
  ;; commodity_t * new_comm;
  ;;
  ;; if ((_keep_price && details.price) ||
  ;;     (_keep_date  && details.date) ||
  ;;     (_keep_tag   && details.tag))
  ;; {
  ;;   new_comm = parent().find_or_create
  ;;     (referent(),
  ;;      annotation_t(_keep_price ? details.price : none,
  ;;                   _keep_date  ? details.date  : none,
  ;;                   _keep_tag   ? details.tag   : none));
  ;; } else {
  ;;   new_comm = parent().find_or_create(base_symbol());
  ;; }
  ;;
  ;; assert(new_comm);
  ;; return *new_comm;
  (assert annotated-commodity)
  (assert (or keep-price keep-date keep-tag)))

(defmethod commodity-symbol ((annotated-commodity annotated-commodity))
  (commodity-symbol (slot-value annotated-commodity 'referent-commodity)))

(defmethod commodity-thousand-marks-p ((annotated-commodity annotated-commodity))
  (commodity-thousand-marks-p
   (slot-value annotated-commodity 'referent-commodity)))

(defmethod display-precision ((annotated-commodity annotated-commodity))
  (display-precision (slot-value annotated-commodity 'referent-commodity)))

(defmethod market-value ((annotated-commodity annotated-commodity) &optional datetime)
  ;; (market-value (slot-value annotated-commodity 'referent-commodity) datetime)
  (assert (or annotated-commodity datetime)))

(defmethod commodity-annotation-empty-p ((annotation commodity-annotation))
  (not (or (commodity-annotation-price annotation)
	   (commodity-annotation-date annotation)
	   (commodity-annotation-tag annotation))))

(defun read-commodity-annotation (in)
  (declare (type (or stream string null) in))
  ;; do {
  ;;   char buf[256];
  ;;   char c = peek_next_nonws(in);
  ;;   if (c == '{') {
  ;;     if (price)
  ;;       throw_(amount_error, "Commodity specifies more than one price");
  ;;
  ;;     in.get(c);
  ;;     READ_INTO(in, buf, 255, c, c != '}');
  ;;     if (c == '}')
  ;;       in.get(c);
  ;;     else
  ;;       throw_(amount_error, "Commodity price lacks closing brace");
  ;;
  ;;     amount_t temp;
  ;;     temp.parse(buf, AMOUNT_PARSE_NO_MIGRATE);
  ;;     temp.in_place_reduce();
  ;;
  ;;     // Since this price will maintain its own precision, make sure
  ;;     // it is at least as large as the base commodity, since the user
  ;;     // may have only specified {$1} or something similar.
  ;;
  ;;     if (temp.has_commodity() &&
  ;;         temp.precision() < temp.commodity().precision())
  ;;       temp = temp.round();    // no need to retain individual precision
  ;;
  ;;     price = temp;
  ;;   }
  ;;   else if (c == '[') {
  ;;     if (date)
  ;;       throw_(amount_error, "Commodity specifies more than one date");
  ;;
  ;;     in.get(c);
  ;;     READ_INTO(in, buf, 255, c, c != ']');
  ;;     if (c == ']')
  ;;       in.get(c);
  ;;     else
  ;;       throw_(amount_error, "Commodity date lacks closing bracket");
  ;;
  ;;     date = parse_datetime(buf);
  ;;   }
  ;;   else if (c == '(') {
  ;;     if (tag)
  ;;       throw_(amount_error, "Commodity specifies more than one tag");
  ;;
  ;;     in.get(c);
  ;;     READ_INTO(in, buf, 255, c, c != ')');
  ;;     if (c == ')')
  ;;       in.get(c);
  ;;     else
  ;;       throw_(amount_error, "Commodity tag lacks closing parenthesis");
  ;;
  ;;     tag = buf;
  ;;   }
  ;;   else {
  ;;     break;
  ;;   }
  ;; } while (true);
  ;;
  ;; DEBUG("amounts.commodities",
  ;;       "Parsed commodity annotations: " << std::endl << *this);
  (assert in))

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
  (assert annotation)
  (format output-stream "~:[~; {~:*~A}~]~:[~; [~:*~A]~]~:[~; (~:*~A)~]"
	  (commodity-annotation-price annotation)
	  (commodity-annotation-date annotation)
	  (commodity-annotation-tag annotation)))

(defmethod commodity-equalp ((a annotated-commodity) (b annotated-commodity))
  ;; // If the base commodities don't match, the game's up.
  ;; if (base != comm.base)
  ;;   return false;
  ;;
  ;; assert(annotated);
  ;; if (! comm.annotated)
  ;;   return false;
  ;;
  ;; if (details != as_annotated_commodity(comm).details)
  ;;   return false;
  ;;
  ;; return true;
  (assert a)
  (assert b))

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
	(setf (slot-value commodity 'qualified-symbol)
	      (concatenate 'string "\"" symbol-name "\"")))

    (let ((commodities-by-serial-list
	   (commodity-pool-commodities-by-serial-list pool)))
      (setf (commodity-serial-number commodity)
	    (1+ (caar (last commodities-by-serial-list))))
      (nconc commodities-by-serial-list
	     (list (cons (commodity-serial-number commodity) commodity))))

    (let ((names-map (commodity-pool-commodities-by-name-map pool)))
      (assert (not (gethash symbol-name names-map)))
      (setf (gethash symbol-name names-map) commodity))))

(defun find-commodity (name &key (pool *default-commodity-pool*)
		       (create-if-not-exists-p nil))
  "Find a COMMODITY identifier by the symbol name found by parsing NAME.
  The NAME can be either a string or an input stream, or nil, in which
  case the name is read from *standard-input*.
  The argument :POOL specifies the commodity pool which will maintain
  this commodity, and by which other code may access it again.
  The resulting COMMODITY object is returned.
  The argument :CREATE-IF-NOT-EXISTS-P indicates whether a new commodity
  should be created if one cannot already be found."
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (let ((by-name-map (commodity-pool-commodities-by-name-map pool)))
    (multiple-value-bind (entry present-p)
	(gethash (if (stringp name)
		     name
		     (commodity-symbol-name name)) by-name-map)
      (if present-p
	  entry
	  (and create-if-not-exists-p
	       (create-commodity name :pool pool))))))

(defun find-commodity-by-serial (serial &key (pool *default-commodity-pool*))
  "Find the commodity with the matching unique SERIAL number.
  nil is returned if no such commodity exists."
  (declare (type fixnum serial))
  (declare (type commodity-pool pool))
  (let ((commodities-by-serial-list
	 (commodity-pool-commodities-by-serial-list pool)))
    (cdr (assoc serial commodities-by-serial-list))))

(defun create-annotated-commodity (name details
				   &key (pool *default-commodity-pool*))
  ;; commodity_t * new_comm = create(symbol);
  ;; if (! new_comm)
  ;;   return NULL;
  ;;
  ;; if (details)
  ;;   return find_or_create(*new_comm, details);
  ;; else
  ;;   return new_comm;
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (assert pool)
  (assert name)
  (assert details))

(defun make-qualified-name (commodity commodity-annotation)
  ;; assert(details);
  ;;
  ;; if (details.price && details.price->sign() < 0)
  ;;   throw_(amount_error, "A commodity's price may not be negative");
  ;;
  ;; std::ostringstream name;
  ;; comm.print(name);
  ;; annotated_commodity_t::write_annotations(name, details);
  ;;
  ;; DEBUG("amounts.commodities", "make_qualified_name for "
  ;;       << comm.qualified_symbol << std::endl << details);
  ;; DEBUG("amounts.commodities", "qualified_name is " << name.str());
  ;;
  ;; return name.str();
  (assert commodity)
  (assert commodity-annotation))

(defun find-annotated-commodity (name details
				 &key (pool *default-commodity-pool*))
  ;; commodity_t * comm = find(symbol);
  ;; if (! comm)
  ;;   return NULL;
  ;;
  ;; if (details) {
  ;;   string name = make_qualified_name(*comm, details);
  ;;
  ;;   if (commodity_t * ann_comm = find(name)) {
  ;;     assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
  ;;     return ann_comm;
  ;;   }
  ;;   return NULL;
  ;; } else {
  ;;   return comm;
  ;; }
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (assert pool)
  (assert name)
  (assert details))

(defun find-or-create-annotated-commodity (name details
					   &key (pool *default-commodity-pool*))
  ;; commodity_t * comm = find(symbol);
  ;; if (! comm)
  ;;   return NULL;
  ;;
  ;; if (details)
  ;;   return find_or_create(*comm, details);
  ;; else
  ;;   return comm;
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (assert pool)
  (assert name)
  (assert details))

(defun create-annotated-commodity-internal (commodity details mapping-key
					    &key (pool *default-commodity-pool*))
  ;; assert(comm);
  ;; assert(details);
  ;; assert(! mapping_key.empty());
  ;;
  ;; std::auto_ptr<commodity_t> commodity
  ;;   (new annotated_commodity_t(&comm, details));
  ;;
  ;; commodity->qualified_symbol = comm.symbol();
  ;; assert(! commodity->qualified_symbol->empty());
  ;;
  ;; DEBUG("amounts.commodities", "Creating annotated commodity "
  ;;       << "symbol " << commodity->symbol()
  ;;       << " key "   << mapping_key << std::endl << details);
  ;;
  ;; // Add the fully annotated name to the map, so that this symbol may
  ;; // quickly be found again.
  ;; commodity->ident        = commodities.size();
  ;; commodity->mapping_key_ = mapping_key;
  ;;
  ;; commodities.push_back(commodity.get());
  ;; return commodity.release();
  (assert pool)
  (assert commodity)
  (assert details)
  (assert mapping-key))

(defun find-or-create-annotated-commodity-internal
    (commodity details &key (pool *default-commodity-pool*))
  ;; assert(comm);
  ;; assert(details);
  ;;
  ;; string name = make_qualified_name(comm, details);
  ;; assert(! name.empty());
  ;;
  ;; if (commodity_t * ann_comm = find(name)) {
  ;;   assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
  ;;   return ann_comm;
  ;; }
  ;; return create(comm, details, name);
  (assert pool)
  (assert commodity)
  (assert details))

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
  (assert value))

(defun integer-to-amount (value)
  (let ((tmp (make-instance 'amount)))
    (setf (slot-value tmp 'quantity) value)
    tmp))

(defun exact-amount (in &key (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (let ((amount
	 (read-amount in :migrate-properties-p nil
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
  (let* ((commodity (amount-commodity amount))
	 (symbol (and commodity (commodity-symbol commodity))))
    (if symbol
	(if (commodity-symbol-needs-quoting-p symbol)
	    (concatenate 'string "\"" (commodity-symbol-name symbol) "\"")
	    (commodity-symbol-name symbol))
	"")))

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

(defmethod value-equal ((left amount) (right amount))
  (value= left right))

(defmethod value-equalp ((left amount) (right amount))
  (assert (and left right)))

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
  ;;   optional<amount_t> amt(commodity().value(moment));
  ;;   if (amt)
  ;;     return (*amt * number()).round();
  ;; } else {
  ;;   throw_(amount_error, "Cannot determine value of an uninitialized amount");
  ;; }
  ;; return none;
  (assert amount)
  (assert datetime))

(defun sign (amount)
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
	    (= 0 (sign (round-to-precision
			(display-precision commodity)))))
	(real-zero-p amount))))

(defmethod real-zero-p ((amount amount))
  (assert amount)
  (unless (slot-boundp amount 'quantity)
    (error 'amount-error :msg
	   "Cannot determine whether an uninitialized amount is zero"))
  (= 0 (slot-value amount 'quantity)))

(defun convert-to-double (amount &optional no-check)
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot convert an uninitialized amount to a double");
  ;;
  ;; mpz_t remainder;
  ;; mpz_init(remainder);
  ;;
  ;; mpz_set(temp, MPZ(quantity));
  ;; mpz_ui_pow_ui(divisor, 10, quantity->prec);
  ;; mpz_tdiv_qr(temp, remainder, temp, divisor);
  ;;
  ;; char * quotient_s  = mpz_get_str(NULL, 10, temp);
  ;; char * remainder_s = mpz_get_str(NULL, 10, remainder);
  ;;
  ;; std::ostringstream num;
  ;; num << quotient_s << '.' << remainder_s;
  ;;
  ;; std::free(quotient_s);
  ;; std::free(remainder_s);
  ;;
  ;; mpz_clear(remainder);
  ;;
  ;; double value = lexical_cast<double>(num.str());
  ;;
  ;; if (! no_check && *this != value)
  ;;   throw_(amount_error, "Conversion of amount to_double loses precision");
  ;;
  ;; return value;
  (assert amount)
  (assert no-check))

(defun convert-to-integer (amount &optional no-check)
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot convert an uninitialized amount to a long");
  ;;
  ;; mpz_set(temp, MPZ(quantity));
  ;; mpz_ui_pow_ui(divisor, 10, quantity->prec);
  ;; mpz_tdiv_q(temp, temp, divisor);
  ;;
  ;; long value = mpz_get_si(temp);
  ;;
  ;; if (! no_check && *this != value)
  ;;   throw_(amount_error, "Conversion of amount to_long loses precision");
  ;;
  ;; return value;
  (assert amount)
  (assert no-check))

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

(defun fits-in-double-p (amount)
  ;; double value = to_double(true);
  ;; return *this == amount_t(value);
  (assert amount))

(defun fits-in-long-p (amount)
  ;; long value = to_long(true);
  ;; return *this == amount_t(value);
  (assert amount))

(defun amount-lessp (left right)
  (minusp (amount-compare left right)))

(defun amount-greaterp (left right)
  (plusp (amount-compare left right)))

(defun amount-quantity (amount)
  ;; if (! has_commodity())
  ;;   return *this;
  ;;
  ;; amount_t temp(*this);
  ;; temp.clear_commodity();
  ;; return temp;
  (assert amount))

(defmethod annotate-commodity ((commodity commodity)
			       (commodity-annotation commodity-annotation))
  (assert commodity)
  (assert commodity-annotation))

(defmethod annotate-commodity ((amount amount)
			       (commodity-annotation commodity-annotation))
  ;; commodity_t *           this_base;
  ;; annotated_commodity_t * this_ann = NULL;
  ;;
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot annotate the commodity of an uninitialized amount");
  ;; else if (! has_commodity())
  ;;   throw_(amount_error, "Cannot annotate an amount with no commodity");
  ;;
  ;; if (commodity().annotated) {
  ;;   this_ann  = &as_annotated_commodity(commodity());
  ;;   this_base = &this_ann->referent();
  ;; } else {
  ;;   this_base = &commodity();
  ;; }
  ;; assert(this_base);
  ;;
  ;; DEBUG("amounts.commodities", "Annotating commodity for amount "
  ;;       << *this << std::endl << details);
  ;;
  ;; if (commodity_t * ann_comm =
  ;;     this_base->parent().find_or_create(*this_base, details))
  ;;   set_commodity(*ann_comm);
  ;; #ifdef ASSERTS_ON
  ;; else
  ;;   assert(false);
  ;; #endif
  ;;
  ;; DEBUG("amounts.commodities", "  Annotated amount is " << *this);
  (assert amount)
  (assert commodity-annotation))

(defmethod commodity-annotated-p ((commodity commodity))
  (slot-value commodity 'annotated-p))

(defmethod commodity-annotated-p ((annotated-commodity annotated-commodity))
  t)

(defmethod commodity-annotated-p ((amount amount))
  (commodity-annotated-p (amount-commodity amount)))

(defmethod commodity-annotation ((commodity commodity))
  (assert commodity))

(defmethod commodity-annotation ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error,
  ;;          "Cannot return commodity annotation details of an uninitialized amount");
  ;;
  ;; assert(! commodity().annotated || as_annotated_commodity(commodity()).details);
  ;;
  ;; if (commodity().annotated) {
  ;;   annotated_commodity_t& ann_comm(as_annotated_commodity(commodity()));
  ;;   return ann_comm.details;
  ;; }
  ;; return annotation_t();
  (assert amount))

(defmethod strip-annotations ((amount amount)
			      &optional keep-price keep-date keep-tag)
  ;; jww (2007-10-17): NYI
  (assert amount)
  (assert (or keep-price keep-date keep-tag)))

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

(defun read-amount (in &key (migrate-properties-p t)
		    (reduce-to-smallest-units-p t)
		    (pool *default-commodity-pool*))
  "Parse an AMOUNT from the input IN, which may be a stream or string.

  If :MIGRATE-PROPERTIES-P is T (the default), any display details noticed in
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
	commodity commodity-newly-created-p
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
	       (or (char= #\{)
		   (char= #\[)
		   (char= #\()))
	  (setq details (read-commodity-annotation in))))

    ;; Now that we have the full commodity symbol, create the commodity object
    ;; it refers to

    (when (and symbol
	       (not (zerop (length (commodity-symbol-name symbol)))))
      (setq commodity (find-commodity symbol :pool pool))
      (unless commodity
	(setq commodity (create-commodity symbol :pool pool)
	      commodity-newly-created-p t))
      (assert commodity)
      (if details
	  (setq commodity (find-or-create-annotated-commodity-internal
			   commodity details :pool pool))))

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

    (when (and commodity
	       (or commodity-newly-created-p
		   migrate-properties-p))
      ;; Migrate the commodity usage details we noticed while parsing
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
		(slot-value amount 'internal-precision))))

    (if negative-p
	(negate* amount))

    (if reduce-to-smallest-units-p
	(smallest-units* amount))

    amount))

(defun read-amount-lightly (in &key (reduce-to-smallest-units-p t)
			    (pool *default-commodity-pool*))
  (read-amount in :migrate-properties-p nil
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun parse-amount (in &key (migrate-properties-p t)
		     (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (read-amount in :migrate-properties-p migrate-properties-p
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun parse-amount-lightly (in &key (reduce-to-smallest-units-p t)
			     (pool *default-commodity-pool*))
  (read-amount in :migrate-properties-p nil
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
	       (commodity-annotated-p commodity))
	  (format-commodity-annotation commodity
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

;; jww (2007-10-15): This requires FFI binding to gdtoa
(defun parse-double (string)
  ;; int    decpt, sign;
  ;; char * buf = dtoa(val, 0, 0, &decpt, &sign, NULL);
  ;; char * result;
  ;; int    len = std::strlen(buf);
  ;;
  ;; if (decpt <= len) {
  ;;   decpt  = len - decpt;
  ;;   result = NULL;
  ;; } else {
  ;;   // There were trailing zeros, which we have to put back on in
  ;;   // order to convert this buffer into an integer.
  ;;
  ;;   int zeroes = decpt - len;
  ;;   result = new char[len + zeroes + 1];
  ;;
  ;;   std::strcpy(result, buf);
  ;;   int i;
  ;;   for (i = 0; i < zeroes; i++)
  ;;     result[len + i] = '0';
  ;;   result[len + i] = '\0';
  ;;
  ;;   decpt = (len - decpt) + zeroes;
  ;; }
  ;;
  ;; if (sign) {
  ;;   char * newbuf = new char[std::strlen(result ? result : buf) + 2];
  ;;   newbuf[0] = '-';
  ;;   std::strcpy(&newbuf[1], result ? result : buf);
  ;;   mpz_set_str(dest, newbuf, 10);
  ;;   checked_array_delete(newbuf);
  ;; } else {
  ;;   mpz_set_str(dest, result ? result : buf, 10);
  ;; }
  ;;
  ;; if (result)
  ;;   checked_array_delete(result);
  ;; freedtoa(buf);
  ;;
  ;; return decpt;
  (assert string))

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
			      &optional keep-price keep-date keep-tag)
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
