;; cambl.lisp

;; This is the Commoditized AMounts and BaLances library.

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

(declaim (optimize debug))

(defpackage :cambl
  (:use :common-lisp)
  (:export :create-commodity))

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

;; Commodities are references to basic commodities, which store the common
;; details.  This is so the commodities USD and $ can both refer to the same
;; underlying kind.

(defstruct commodity-price-history
  (prices nil :type list)
  (last-lookup nil :type (or datetime null)))

(defclass basic-commodity ()
  ((symbol :initarg :symbol :type commodity-symbol)
   (name :accessor commodity-name)
   (note :accessor commodity-note)
   (smaller-unit-equivalence :accessor smaller-unit-equivalence)
   (larger-unit-equivalence :accessor larger-unit-equivalence)
   (thousand-marks-p :initarg :thousand-marks-p :type boolean)
   (no-market-price-p :initarg :no-market-price-p :type boolean)
   (builtin-p :initarg :builtin-p :type boolean)
   (display-precision :initform 0 :type fixnum)
   (price-history :accessor commodity-price-history
		  :type commodity-price-history)))

(defstruct commodity-pool
  (commodities-by-name-map (make-hash-table :test 'equal) :type hash-table)
  (commodities-by-serial-list '((0 . nil)) :type list)
  (default-commodity nil))

(defparameter *default-commodity-pool* (make-commodity-pool))

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
	      :type commodity)
   (quantity :type integer)
   (display-precision :type fixnum)
   (internal-precision :type fixnum)
   (keep-precision :initform nil :type boolean)
   ;;value-origins-list
   ;; (commodity-pool :accessor amount-commodity-pool :initarg :pool
   ;;   :type commodity-pool)
   (keep-base :allocation :class :initform nil :type boolean)
   (keep-price :allocation :class :initform nil :type boolean)
   (keep-date :allocation :class :initform nil :type boolean)
   (keep-tag :allocation :class :initform nil :type boolean)
   (stream-fullstrings :allocation :class :initform nil :type boolean)))

(defclass balance ()
  (amounts-map))

(defclass annotated-commodity (commodity)
  ((referent-commodity :type commodity)
   (annotation :type commodity-annotation)))

(defstruct commodity-annotation
  (price nil :type (or amount null))
  (date nil :type (or datetime null))
  (tag nil :type (or string null)))

(defgeneric commodity-symbol (commodity))
(defgeneric commodity-base (commodity))
(defgeneric commodity-equal (commodity commodity))
(defgeneric commodity-equalp (commodity commodity))
(defgeneric commodity-thousand-marks-p (commodity))
(defgeneric commodity-no-market-price-p (commodity))
(defgeneric commodity-builtin-p (commodity))
(defgeneric commodity-display-precision (commodity))
(defgeneric strip-annotations (commodity &optional keep-price keep-date keep-tag))
(defgeneric market-value (commodity &optional datetime))
(defgeneric commodity-annotated-p (item))
(defgeneric commodity-annotation-empty-p (annotation))
(defgeneric negate* (value))
(defgeneric negate (value))
(defgeneric zero-p (amount))
(defgeneric real-zero-p (amount))
(defgeneric annotate-commodity (commodity annotation))
(defgeneric commodity-annotation (item))
(defgeneric value= (value value))
(defgeneric add-to-balance (balance value))
(defgeneric add (value value))
(defgeneric subtract (value value))
(defgeneric multiply (value value))
(defgeneric divide (value value))
(defgeneric absolute (value))
(defgeneric round-to-precision (value &optional precision))
(defgeneric unround (value))
(defgeneric smaller-units* (value))
(defgeneric smaller-units (value))
(defgeneric larger-units* (value))
(defgeneric larger-units (value))
(defgeneric convert-to-amount (value))
(defgeneric convert-to-string (value))
(defgeneric convert-to-fullstring (value))

(defgeneric amount-in-balance (balance commodity))

;;; Functions:

(defun symbol-name-needs-quoting-p (name)
  "Return T if the given symbol NAME requires quoting."
  (declare (type string name))
  (loop for c across name do
       (and (aref +invalid-symbol-chars+ (char-code c))
	    (return t))))

(defun get-input-stream (&optional in)
  (declare (type (or stream string null) in))
  (if in
      (if (typep in 'stream)
	  in
	  (make-string-input-stream in))
      *standard-input*))

(defun parse-commodity-symbol (&optional in)
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
		      (if (aref +invalid-symbol-chars+ (char-code c))
			  (setf needs-quoting-p t))
		      (write-char c buf)))
		(error "Quoted commodity symbol lacks closing quote"))))
	(do ((c (read-char in) (read-char in nil 'the-end)))
	    ((not (characterp c)))
	  (if (aref +invalid-symbol-chars+ (char-code c))
	      (progn
		(unread-char c in)
		(return))
	      (write-char c buf))))
    (make-commodity-symbol :name (get-output-stream-string buf)
			   :needs-quoting-p needs-quoting-p)))

;; The commodity and annotated-commodity classes are the main interface class
;; for dealing with commodities themselves (which most people will never do).

(defmethod commodity-symbol ((basic-commodity basic-commodity))
  (slot-value basic-commodity 'symbol))

(defmethod commodity-base ((basic-commodity basic-commodity))
  basic-commodity)

(defmethod commodity-base ((commodity commodity))
  (slot-value commodity 'basic-commodity))

(defmethod commodity-thousand-marks-p ((basic-commodity basic-commodity))
  (slot-value basic-commodity 'thousand-marks-p))

(defmethod commodity-thousand-marks-p ((commodity commodity))
  (commodity-thousand-marks-p (slot-value commodity 'basic-commodity)))

(defmethod commodity-display-precision ((basic-commodity basic-commodity))
  (slot-value basic-commodity 'display-precision))

(defmethod commodity-display-precision ((commodity commodity))
  (commodity-display-precision (slot-value commodity 'basic-commodity)))

(defmethod commodity-symbol ((commodity commodity))
  (if (slot-value commodity 'has-qualified-symbol-p)
      (slot-value commodity 'qualified-symbol)
      (commodity-symbol (slot-value commodity 'basic-commodity))))

(defmethod commodity-equalp ((a commodity) (b commodity))
  "Two commodities are considered EQUALP if they refer to the same base."
  (assert (nth-value 0 (subtypep (type-of a) 'commodity)))
  (assert (nth-value 0 (subtypep (type-of b) 'commodity)))
  (eq (slot-value a 'basic-commodity) (slot-value b 'basic-commodity)))

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

(defun compare-commodity-representations (left right)
  "Return T if commodity LEFT should be sorted before RIGHT."
  ;; commodity_t& leftcomm(left->commodity());
  ;; commodity_t& rightcomm(right->commodity());
  ;;
  ;; int cmp = leftcomm.base_symbol().compare(rightcomm.base_symbol());
  ;; if (cmp != 0)
  ;;   return cmp < 0;
  ;;
  ;; if (! leftcomm.annotated) {
  ;;   assert(rightcomm.annotated);
  ;;   return true;
  ;; }
  ;; else if (! rightcomm.annotated) {
  ;;   assert(leftcomm.annotated);
  ;;   return false;
  ;; }
  ;; else {
  ;;   annotated_commodity_t& aleftcomm(static_cast<annotated_commodity_t&>(leftcomm));
  ;;   annotated_commodity_t& arightcomm(static_cast<annotated_commodity_t&>(rightcomm));
  ;;
  ;;   if (! aleftcomm.details.price && arightcomm.details.price)
  ;;     return true;
  ;;   if (aleftcomm.details.price && ! arightcomm.details.price)
  ;;     return false;
  ;;
  ;;   if (aleftcomm.details.price && arightcomm.details.price) {
  ;;     amount_t leftprice(*aleftcomm.details.price);
  ;;     leftprice.in_place_reduce();
  ;;     amount_t rightprice(*arightcomm.details.price);
  ;;     rightprice.in_place_reduce();
  ;;
  ;;     if (leftprice.commodity() == rightprice.commodity()) {
  ;;       return (leftprice - rightprice).sign() < 0;
  ;;     } else {
  ;;       // Since we have two different amounts, there's really no way
  ;;       // to establish a true sorting order; we'll just do it based
  ;;       // on the numerical values.
  ;;       leftprice.clear_commodity();
  ;;       rightprice.clear_commodity();
  ;;       return (leftprice - rightprice).sign() < 0;
  ;;     }
  ;;   }
  ;;
  ;;   if (! aleftcomm.details.date && arightcomm.details.date)
  ;;     return true;
  ;;   if (aleftcomm.details.date && ! arightcomm.details.date)
  ;;     return false;
  ;;
  ;;   if (aleftcomm.details.date && arightcomm.details.date) {
  ;;     duration_t diff = *aleftcomm.details.date - *arightcomm.details.date;
  ;;     return diff.is_negative();
  ;;   }
  ;;
  ;;   if (! aleftcomm.details.tag && arightcomm.details.tag)
  ;;     return true;
  ;;   if (aleftcomm.details.tag && ! arightcomm.details.tag)
  ;;     return false;
  ;;
  ;;   if (aleftcomm.details.tag && arightcomm.details.tag)
  ;;     return *aleftcomm.details.tag < *arightcomm.details.tag;
  ;;
  ;;   assert(false);
  ;;   return true;
  ;; }
  (assert (and left right)))

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

(defmethod commodity-symbol ((annotated-commodity annotated-commodity))
  (commodity-symbol (slot-value annotated-commodity 'referent-commodity)))

(defmethod commodity-thousand-marks-p ((annotated-commodity annotated-commodity))
  (commodity-thousand-marks-p
   (slot-value annotated-commodity 'referent-commodity)))

(defmethod commodity-display-precision ((annotated-commodity annotated-commodity))
  (commodity-display-precision
   (slot-value annotated-commodity 'referent-commodity)))

(defmethod market-value ((annotated-commodity annotated-commodity) &optional datetime)
  ;; (market-value (slot-value annotated-commodity 'referent-commodity) datetime)
  (assert (or annotated-commodity datetime)))

(defmethod commodity-annotation-empty-p ((annotation commodity-annotation))
  (not (or (commodity-annotation-price annotation)
	   (commodity-annotation-date annotation)
	   (commodity-annotation-tag annotation))))

(defun parse-commodity-annotation (in)
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

(defun commodity-annotation-string (annotation &optional out)
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
  (format out "~:[~; {~:*~A}~]~:[~; [~:*~A]~]~:[~; (~:*~A)~]"
	  (commodity-annotation-price annotation)
	  (commodity-annotation-date annotation)
	  (commodity-annotation-tag annotation)))

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
		     (parse-commodity-symbol name)
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

(defun exact-amount (string)
  ;; amount_t temp;
  ;; temp.parse(value, AMOUNT_PARSE_NO_MIGRATE);
  ;; return temp;
  (assert string))

(defun copy-amount (amount)
  ;; TRACE_CTOR(amount_t, "copy");
  ;; if (amt.quantity)
  ;;   _copy(amt);
  ;; else
  ;;   commodity_ = NULL;
  (assert amount))

(defun compare-amounts (amount other)
  ;; if (! quantity || ! amt.quantity) {
  ;;   if (quantity)
  ;;     throw_(amount_error, "Cannot compare an amount to an uninitialized amount");
  ;;   else if (amt.quantity)
  ;;     throw_(amount_error, "Cannot compare an uninitialized amount to an amount");
  ;;   else
  ;;     throw_(amount_error, "Cannot compare two uninitialized amounts");
  ;; }
  ;;
  ;; if (has_commodity() && amt.has_commodity() &&
  ;;     commodity() != amt.commodity())
  ;;   throw_(amount_error,
  ;;          "Cannot compare amounts with different commodities: " <<
  ;;          commodity().symbol() << " and " << amt.commodity().symbol());
  ;;
  ;; if (quantity->prec == amt.quantity->prec) {
  ;;   return mpz_cmp(MPZ(quantity), MPZ(amt.quantity));
  ;; }
  ;; else if (quantity->prec < amt.quantity->prec) {
  ;;   amount_t t(*this);
  ;;   t._resize(amt.quantity->prec);
  ;;   return mpz_cmp(MPZ(t.quantity), MPZ(amt.quantity));
  ;; }
  ;; else {
  ;;   amount_t t = amt;
  ;;   t._resize(quantity->prec);
  ;;   return mpz_cmp(MPZ(quantity), MPZ(t.quantity));
  ;; }
  (assert (and amount other)))

(defmethod value= ((left amount) (right amount))
  (assert (and left right)))

(defmethod add ((left amount) (right amount))
  ;; if (! quantity || ! amt.quantity) {
  ;;   if (quantity)
  ;;     throw_(amount_error, "Cannot add an amount to an uninitialized amount");
  ;;   else if (amt.quantity)
  ;;     throw_(amount_error, "Cannot add an uninitialized amount to an amount");
  ;;   else
  ;;     throw_(amount_error, "Cannot add two uninitialized amounts");
  ;; }
  ;;
  ;; if (commodity() != amt.commodity())
  ;;   throw_(amount_error,
  ;;          "Adding amounts with different commodities: " <<
  ;;          (has_commodity() ? commodity().symbol() : "NONE") <<
  ;;          " != " <<
  ;;          (amt.has_commodity() ? amt.commodity().symbol() : "NONE"));
  ;;
  ;; _dup();
  ;;
  ;; if (quantity->prec == amt.quantity->prec) {
  ;;   mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  ;; }
  ;; else if (quantity->prec < amt.quantity->prec) {
  ;;   _resize(amt.quantity->prec);
  ;;   mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  ;; }
  ;; else {
  ;;   amount_t t = amt;
  ;;   t._resize(quantity->prec);
  ;;   mpz_add(MPZ(quantity), MPZ(quantity), MPZ(t.quantity));
  ;; }
  ;;
  ;; return *this;
  (assert (and left right)))

(defmethod subtract ((left amount) (right amount))
  ;; if (! quantity || ! amt.quantity) {
  ;;   if (quantity)
  ;;     throw_(amount_error, "Cannot subtract an amount from an uninitialized amount");
  ;;   else if (amt.quantity)
  ;;     throw_(amount_error, "Cannot subtract an uninitialized amount from an amount");
  ;;   else
  ;;     throw_(amount_error, "Cannot subtract two uninitialized amounts");
  ;; }
  ;;
  ;; if (commodity() != amt.commodity())
  ;;   throw_(amount_error,
  ;;          "Subtracting amounts with different commodities: " <<
  ;;          (has_commodity() ? commodity().symbol() : "NONE") <<
  ;;          " != " <<
  ;;          (amt.has_commodity() ? amt.commodity().symbol() : "NONE"));
  ;;
  ;; _dup();
  ;;
  ;; if (quantity->prec == amt.quantity->prec) {
  ;;   mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  ;; }
  ;; else if (quantity->prec < amt.quantity->prec) {
  ;;   _resize(amt.quantity->prec);
  ;;   mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  ;; }
  ;; else {
  ;;   amount_t t = amt;
  ;;   t._resize(quantity->prec);
  ;;   mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(t.quantity));
  ;; }
  ;;
  ;; return *this;
  (assert (and left right)))

(defmethod multiply ((left amount) (right amount))
  ;; void mpz_round(mpz_t out, mpz_t value, int value_prec, int round_prec)
  ;; {
  ;;   // Round `value', with an encoding precision of `value_prec', to a
  ;;   // rounded value with precision `round_prec'.  Result is stored in
  ;;   // `out'.
  ;;
  ;;   assert(value_prec > round_prec);
  ;;
  ;;   mpz_t quotient;
  ;;   mpz_t remainder;
  ;;
  ;;   mpz_init(quotient);
  ;;   mpz_init(remainder);
  ;;
  ;;   mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
  ;;   mpz_tdiv_qr(quotient, remainder, value, divisor);
  ;;   mpz_divexact_ui(divisor, divisor, 10);
  ;;   mpz_mul_ui(divisor, divisor, 5);
  ;;
  ;;   if (mpz_sgn(remainder) < 0) {
  ;;     mpz_neg(divisor, divisor);
  ;;     if (mpz_cmp(remainder, divisor) < 0) {
  ;;       mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
  ;;       mpz_add(remainder, divisor, remainder);
  ;;       mpz_ui_sub(remainder, 0, remainder);
  ;;       mpz_add(out, value, remainder);
  ;;     } else {
  ;;       mpz_sub(out, value, remainder);
  ;;     }
  ;;   } else {
  ;;     if (mpz_cmp(remainder, divisor) >= 0) {
  ;;       mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
  ;;       mpz_sub(remainder, divisor, remainder);
  ;;       mpz_add(out, value, remainder);
  ;;     } else {
  ;;       mpz_sub(out, value, remainder);
  ;;     }
  ;;   }
  ;;   mpz_clear(quotient);
  ;;   mpz_clear(remainder);
  ;;
  ;;   // chop off the rounded bits
  ;;   mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
  ;;   mpz_tdiv_q(out, out, divisor);
  ;; }
  ;;
  ;; if (! quantity || ! amt.quantity) {
  ;;   if (quantity)
  ;;     throw_(amount_error, "Cannot multiply an amount by an uninitialized amount");
  ;;   else if (amt.quantity)
  ;;     throw_(amount_error, "Cannot multiply an uninitialized amount by an amount");
  ;;   else
  ;;     throw_(amount_error, "Cannot multiply two uninitialized amounts");
  ;; }
  ;;
  ;; if (has_commodity() && amt.has_commodity() &&
  ;;     commodity() != amt.commodity())
  ;;   throw_(amount_error,
  ;;          "Multiplying amounts with different commodities: " <<
  ;;          (has_commodity() ? commodity().symbol() : "NONE") <<
  ;;          " != " <<
  ;;          (amt.has_commodity() ? amt.commodity().symbol() : "NONE"));
  ;;
  ;; _dup();
  ;;
  ;; mpz_mul(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  ;; quantity->prec += amt.quantity->prec;
  ;;
  ;; if (! has_commodity())
  ;;   commodity_ = amt.commodity_;
  ;;
  ;; if (has_commodity() && ! (quantity->has_flags(BIGINT_KEEP_PREC))) {
  ;;   precision_t comm_prec = commodity().precision();
  ;;   if (quantity->prec > comm_prec + 6U) {
  ;;     mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, comm_prec + 6U);
  ;;     quantity->prec = comm_prec + 6U;
  ;;   }
  ;; }
  ;;
  ;; return *this;
  (assert (and left right)))

(defmethod divide ((left amount) (right amount))
  ;; if (! quantity || ! amt.quantity) {
  ;;   if (quantity)
  ;;     throw_(amount_error, "Cannot divide an amount by an uninitialized amount");
  ;;   else if (amt.quantity)
  ;;     throw_(amount_error, "Cannot divide an uninitialized amount by an amount");
  ;;   else
  ;;     throw_(amount_error, "Cannot divide two uninitialized amounts");
  ;; }
  ;;
  ;; if (has_commodity() && amt.has_commodity() &&
  ;;     commodity() != amt.commodity())
  ;;   throw_(amount_error,
  ;;          "Dividing amounts with different commodities: " <<
  ;;          (has_commodity() ? commodity().symbol() : "NONE") <<
  ;;          " != " <<
  ;;          (amt.has_commodity() ? amt.commodity().symbol() : "NONE"));
  ;;
  ;; if (! amt)
  ;;   throw_(amount_error, "Divide by zero");
  ;;
  ;; _dup();
  ;;
  ;; // Increase the value's precision, to capture fractional parts after
  ;; // the divide.  Round up in the last position.
  ;;
  ;; mpz_ui_pow_ui(divisor, 10, (2 * amt.quantity->prec) + quantity->prec + 7U);
  ;; mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  ;; mpz_tdiv_q(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  ;; quantity->prec += amt.quantity->prec + quantity->prec + 7U;
  ;;
  ;; mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, quantity->prec - 1);
  ;; quantity->prec -= 1;
  ;;
  ;; if (! has_commodity())
  ;;   commodity_ = amt.commodity_;
  ;;
  ;; // If this amount has a commodity, and we're not dealing with plain
  ;; // numbers, or internal numbers (which keep full precision at all
  ;; // times), then round the number to within the commodity's precision
  ;; // plus six places.
  ;;
  ;; if (has_commodity() && ! (quantity->has_flags(BIGINT_KEEP_PREC))) {
  ;;   precision_t comm_prec = commodity().precision();
  ;;   if (quantity->prec > comm_prec + 6U) {
  ;;     mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, comm_prec + 6U);
  ;;     quantity->prec = comm_prec + 6U;
  ;;   }
  ;; }
  ;;
  ;; return *this;
  (assert (and left right)))

(defmethod negate* ((amount amount))
  (setf (slot-value amount 'quantity)
	(- (slot-value amount 'quantity))))

(defmethod negate ((amount amount))
  (let ((tmp (copy-amount amount)))
    ;; (negate* tmp)
    (assert tmp)
    ))

(defmethod absolute ((amount amount))
  ;; if (sign() < 0)
  ;;   return negate();
  ;; return *this;
  (assert amount))

(defmethod round-to-precision ((amount amount) &optional precision)
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot round an uninitialized amount");
  ;;
  ;; if (! has_commodity())
  ;;   return *this;
  ;;
  ;; return round(commodity().precision());

  ;; ;; with a precision specified:

  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot round an uninitialized amount");
  ;;
  ;; amount_t t(*this);
  ;;
  ;; if (quantity->prec <= prec) {
  ;;   if (quantity && quantity->has_flags(BIGINT_KEEP_PREC)) {
  ;;     t._dup();
  ;;     t.quantity->drop_flags(BIGINT_KEEP_PREC);
  ;;   }
  ;;   return t;
  ;; }
  ;;
  ;; t._dup();
  ;;
  ;; mpz_round(MPZ(t.quantity), MPZ(t.quantity), t.quantity->prec, prec);
  ;;
  ;; t.quantity->prec = prec;
  ;; t.quantity->drop_flags(BIGINT_KEEP_PREC);
  ;;
  ;; return t;
  (assert amount)
  (assert precision))

(defmethod unround ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot unround an uninitialized amount");
  ;; else if (quantity->has_flags(BIGINT_KEEP_PREC))
  ;;   return *this;
  ;;
  ;; amount_t t(*this);
  ;; t._dup();
  ;; t.quantity->add_flags(BIGINT_KEEP_PREC);
  ;;
  ;; return t;
  (assert amount))

(defmethod smaller-units* ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot reduce an uninitialized amount");
  ;;
  ;; while (commodity_ && commodity().smaller()) {
  ;;   *this *= commodity().smaller()->number();
  ;;   commodity_ = commodity().smaller()->commodity_;
  ;; }
  ;; return *this;
  (assert amount))

(defmethod smaller-units ((amount amount))
  (let ((tmp (copy-amount amount)))
    ;; (reduce-in-place tmp)
    (assert tmp)
    ))

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
    ;; (unreduce-in-place tmp)
    (assert tmp)
    ))

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
  (let ((quantity (slot-value amount 'quantity)))
    (if (< quantity 0)
	-1
	(if (> quantity 0)
	    1
	    0))))

(defmethod zero-p ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot determine sign if an uninitialized amount is zero");
  ;;
  ;; if (has_commodity()) {
  ;;   if (quantity->prec <= commodity().precision())
  ;;     return is_realzero();
  ;;   else
  ;;     return round(commodity().precision()).sign() == 0;
  ;; }
  ;; return is_realzero();
  (assert amount))

(defmethod real-zero-p ((amount amount))
  (assert amount))

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

(defmethod convert-to-string ((amount amount))
  ;; std::ostringstream bufstream;
  ;; print(bufstream);
  ;; return bufstream.str();
  (assert amount))

(defmethod convert-to-fullstring ((amount amount))
  ;; std::ostringstream bufstream;
  ;; print(bufstream, false, true);
  ;; return bufstream.str();
  (assert amount))

(defun quantity-string (amount)
  ;; std::ostringstream bufstream;
  ;; print(bufstream, true);
  ;; return bufstream.str();
  (assert amount))

(defun fits-in-double-p (amount)
  ;; double value = to_double(true);
  ;; return *this == amount_t(value);
  (assert amount))

(defun fits-in-long-p (amount)
  ;; long value = to_long(true);
  ;; return *this == amount_t(value);
  (assert amount))

(defun amount-number (amount)
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

(defmethod commodity-annotated-p ((amount amount))
  ;; if (! quantity)
  ;;   throw_(amount_error,
  ;;          "Cannot determine if an uninitialized amount's commodity is annotated");
  ;;
  ;; assert(! commodity().annotated || as_annotated_commodity(commodity()).details);
  ;; return commodity().annotated;
  (assert amount))

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
  (assert amount)
  (assert (or keep-price keep-date keep-tag)))

(defun parse-amount-quantity (in)
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

(defun parse-amount (in &key (migrate-properties-p t)
		     (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  ;; The possible syntax for an amount is:
  ;; 
  ;;   [-]NUM[ ]SYM [@ PRICE]
  ;;   SYM[ ][-]NUM [@ PRICE]
  ;;
  ;; string       symbol;
  ;; string       quant;
  ;; annotation_t details;
  ;; bool         negative   = false;
  ;;
  ;; commodity_t::flags_t comm_flags = COMMODITY_STYLE_DEFAULTS;
  ;;
  ;; char c = peek_next_nonws(in);
  ;; if (c == '-') {
  ;;   negative = true;
  ;;   in.get(c);
  ;;   c = peek_next_nonws(in);
  ;; }
  ;;
  ;; char n;
  ;; if (std::isdigit(c)) {
  ;;   parse_quantity(in, quant);
  ;;
  ;;   if (! in.eof() && ((n = in.peek()) != '\n')) {
  ;;     if (std::isspace(n))
  ;;       comm_flags |= COMMODITY_STYLE_SEPARATED;
  ;;
  ;;     commodity_t::parse_symbol(in, symbol);
  ;;
  ;;     if (! symbol.empty())
  ;;       comm_flags |= COMMODITY_STYLE_SUFFIXED;
  ;;
  ;;     if (! in.eof() && ((n = in.peek()) != '\n'))
  ;;       details.parse(in);
  ;;   }
  ;; } else {
  ;;   commodity_t::parse_symbol(in, symbol);
  ;;
  ;;   if (! in.eof() && ((n = in.peek()) != '\n')) {
  ;;     if (std::isspace(in.peek()))
  ;;       comm_flags |= COMMODITY_STYLE_SEPARATED;
  ;;
  ;;     parse_quantity(in, quant);
  ;;
  ;;     if (! quant.empty() && ! in.eof() && ((n = in.peek()) != '\n'))
  ;;       details.parse(in);
  ;;   }
  ;; }
  ;;
  ;; if (quant.empty())
  ;;   throw_(amount_error, "No quantity specified for amount");
  ;;
  ;; // Allocate memory for the amount's quantity value.  We have to
  ;; // monitor the allocation in an auto_ptr because this function gets
  ;; // called sometimes from amount_t's constructor; and if there is an
  ;; // exeception thrown by any of the function calls after this point,
  ;; // the destructor will never be called and the memory never freed.
  ;;
  ;; std::auto_ptr<bigint_t> safe_holder;
  ;;
  ;; if (! quantity) {
  ;;   quantity = new bigint_t;
  ;;   safe_holder.reset(quantity);
  ;; }
  ;; else if (quantity->ref > 1) {
  ;;   _release();
  ;;   quantity = new bigint_t;
  ;;   safe_holder.reset(quantity);
  ;; }
  ;;
  ;; // Create the commodity if has not already been seen, and update the
  ;; // precision if something greater was used for the quantity.
  ;;
  ;; bool newly_created = false;
  ;;
  ;; if (symbol.empty()) {
  ;;   commodity_ = NULL;
  ;; } else {
  ;;   commodity_ = current_pool->find(symbol);
  ;;   if (! commodity_) {
  ;;     commodity_ = current_pool->create(symbol);
  ;;     newly_created = true;
  ;;   }
  ;;   assert(commodity_);
  ;;
  ;;   if (details)
  ;;     commodity_ = current_pool->find_or_create(*commodity_, details);
  ;; }
  ;;
  ;; // Determine the precision of the amount, based on the usage of
  ;; // comma or period.
  ;;
  ;; string::size_type last_comma  = quant.rfind(',');
  ;; string::size_type last_period = quant.rfind('.');
  ;;
  ;; if (last_comma != string::npos && last_period != string::npos) {
  ;;   comm_flags |= COMMODITY_STYLE_THOUSANDS;
  ;;   if (last_comma > last_period) {
  ;;     comm_flags |= COMMODITY_STYLE_EUROPEAN;
  ;;     quantity->prec = quant.length() - last_comma - 1;
  ;;   } else {
  ;;     quantity->prec = quant.length() - last_period - 1;
  ;;   }
  ;; }
  ;; else if (last_comma != string::npos &&
  ;;          commodity().has_flags(COMMODITY_STYLE_EUROPEAN)) {
  ;;   quantity->prec = quant.length() - last_comma - 1;
  ;; }
  ;; else if (last_period != string::npos &&
  ;;          ! (commodity().has_flags(COMMODITY_STYLE_EUROPEAN))) {
  ;;   quantity->prec = quant.length() - last_period - 1;
  ;; }
  ;; else {
  ;;   quantity->prec = 0;
  ;; }
  ;;
  ;; // Set the commodity's flags and precision accordingly
  ;;
  ;; if (commodity_ && (newly_created || ! (flags & AMOUNT_PARSE_NO_MIGRATE))) {
  ;;   commodity().add_flags(comm_flags);
  ;;
  ;;   if (quantity->prec > commodity().precision())
  ;;     commodity().set_precision(quantity->prec);
  ;; }
  ;;
  ;; // Setup the amount's own flags
  ;;
  ;; if (flags & AMOUNT_PARSE_NO_MIGRATE)
  ;;   quantity->add_flags(BIGINT_KEEP_PREC);
  ;;
  ;; // Now we have the final number.  Remove commas and periods, if
  ;; // necessary.
  ;;
  ;; if (last_comma != string::npos || last_period != string::npos) {
  ;;   int          len = quant.length();
  ;;   char *       buf = new char[len + 1];
  ;;   const char * p   = quant.c_str();
  ;;   char *       t   = buf;
  ;;
  ;;   while (*p) {
  ;;     if (*p == ',' || *p == '.')
  ;;       p++;
  ;;     *t++ = *p++;
  ;;   }
  ;;   *t = '\0';
  ;;
  ;;   mpz_set_str(MPZ(quantity), buf, 10);
  ;;   checked_array_delete(buf);
  ;; } else {
  ;;   mpz_set_str(MPZ(quantity), quant.c_str(), 10);
  ;; }
  ;;
  ;; if (negative)
  ;;   in_place_negate();
  ;;
  ;; if (! (flags & AMOUNT_PARSE_NO_REDUCE))
  ;;   in_place_reduce();
  ;;
  ;; safe_holder.release();        // `this->quantity' owns the pointer
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
	  (setq quantity (parse-amount-quantity in))
	  (assert quantity)

	  (let ((c (peek-char-in-line in)))
	    (if (and (characterp c)
		     (char= #\Space c))
		(setq connected-p nil))
	    (let ((n (peek-char-in-line in t)))
	      (when (and (characterp n)
			 (not (char= #\Newline n)))
		(setq symbol (parse-commodity-symbol in))
		(if symbol
		    (setq prefixed-p nil))))))
	(progn
	  (setq symbol (parse-commodity-symbol in))
	  (if (char= #\Space (peek-char nil in))
	      (setq connected-p nil))
	  (let ((n (peek-char-in-line in t)))
	    (if (and (characterp n)
		     (not (char= #\Newline n)))
		(setq quantity (parse-amount-quantity in))
		(error "No quantity specified for amount")))))

    (let ((c (peek-char-in-line in t)))
      (if (and (characterp c)
	       (or (char= #\{)
		   (char= #\[)
		   (char= #\()))
	  (setq details (parse-commodity-annotation in))))

    ;; Now that we have the full commodity symbol, create the commodity object
    ;; it refers to

    (unless (= 0 (length (commodity-symbol-name symbol)))
      (setq commodity (find-commodity symbol :pool pool))
      (unless commodity
	(setq commodity (create-commodity symbol :pool pool)
	      commodity-newly-created-p t))
      (assert commodity)
      (if details
	  (setq commodity
		(find-or-create-annotated-commodity-internal
		 commodity details :pool pool))))

    ;; Determine the precision of the amount, based on the usage of
    ;; comma or period.

    (setq amount (make-instance 'amount :commodity commodity))

    (let ((last-comma (position #\, quantity :from-end t))
	  (last-period (position #\. quantity :from-end t)))
      (cond ((and last-comma last-period)
	     (setq thousand-marks-p t)
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
      (setf (slot-value (commodity-base commodity) 'thousand-marks-p)
	    thousand-marks-p)

      (if (> (slot-value amount 'internal-precision)
	     (commodity-display-precision commodity))
	  (setf (slot-value (commodity-base commodity) 'display-precision)
		(slot-value amount 'internal-precision))))

    (unless migrate-properties-p
      (setf (slot-value amount 'keep-precision) t))

    (if negative-p
	(negate* amount))

    (if reduce-to-smallest-units-p
	(smaller-units* amount))

    amount))

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
(defun print-amount (amount &optional out omit-commodity full-precision)
  ;; if (! quantity)
  ;;   throw_(amount_error, "Cannot write out an uninitialized amount");
  ;;
  ;; amount_t base(*this);
  ;; if (! amount_t::keep_base)
  ;;   base.in_place_unreduce();
  ;;
  ;; std::ostringstream out;
  ;;
  ;; mpz_t quotient;
  ;; mpz_t rquotient;
  ;; mpz_t remainder;
  ;;
  ;; mpz_init(quotient);
  ;; mpz_init(rquotient);
  ;; mpz_init(remainder);
  ;;
  ;; bool negative = false;
  ;;
  ;; // Ensure the value is rounded to the commodity's precision before
  ;; // outputting it.  NOTE: `rquotient' is used here as a temp variable!
  ;;
  ;; commodity_t& comm(base.commodity());
  ;; precision_t  precision = 0;
  ;;
  ;; if (quantity) {
  ;;   if (! comm || full_precision || base.quantity->has_flags(BIGINT_KEEP_PREC)) {
  ;;     mpz_ui_pow_ui(divisor, 10, base.quantity->prec);
  ;;     mpz_tdiv_qr(quotient, remainder, MPZ(base.quantity), divisor);
  ;;     precision = base.quantity->prec;
  ;;   }
  ;;   else if (comm.precision() < base.quantity->prec) {
  ;;     mpz_round(rquotient, MPZ(base.quantity), base.quantity->prec,
  ;;               comm.precision());
  ;;     mpz_ui_pow_ui(divisor, 10, comm.precision());
  ;;     mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
  ;;     precision = comm.precision();
  ;;   }
  ;;   else if (comm.precision() > base.quantity->prec) {
  ;;     mpz_ui_pow_ui(divisor, 10, comm.precision() - base.quantity->prec);
  ;;     mpz_mul(rquotient, MPZ(base.quantity), divisor);
  ;;     mpz_ui_pow_ui(divisor, 10, comm.precision());
  ;;     mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
  ;;     precision = comm.precision();
  ;;   }
  ;;   else if (base.quantity->prec) {
  ;;     mpz_ui_pow_ui(divisor, 10, base.quantity->prec);
  ;;     mpz_tdiv_qr(quotient, remainder, MPZ(base.quantity), divisor);
  ;;     precision = base.quantity->prec;
  ;;   }
  ;;   else {
  ;;     mpz_set(quotient, MPZ(base.quantity));
  ;;     mpz_set_ui(remainder, 0);
  ;;     precision = 0;
  ;;   }
  ;;
  ;;   if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0) {
  ;;     negative = true;
  ;;
  ;;     mpz_abs(quotient, quotient);
  ;;     mpz_abs(remainder, remainder);
  ;;   }
  ;;   mpz_set(rquotient, remainder);
  ;; }
  ;;
  ;; if (! omit_commodity && ! comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
  ;;   comm.print(out);
  ;;   if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
  ;;     out << " ";
  ;; }
  ;;
  ;; if (negative)
  ;;   out << "-";
  ;;
  ;; if (! quantity || mpz_sgn(quotient) == 0) {
  ;;   out << '0';
  ;; }
  ;; else if (omit_commodity || ! comm.has_flags(COMMODITY_STYLE_THOUSANDS)) {
  ;;   char * p = mpz_get_str(NULL, 10, quotient);
  ;;   out << p;
  ;;   std::free(p);
  ;; }
  ;; else {
  ;;   std::list<string> strs;
  ;;   char buf[4];
  ;;
  ;;   for (int powers = 0; true; powers += 3) {
  ;;     if (powers > 0) {
  ;;       mpz_ui_pow_ui(divisor, 10, powers);
  ;;       mpz_tdiv_q(temp, quotient, divisor);
  ;;       if (mpz_sgn(temp) == 0)
  ;;         break;
  ;;       mpz_tdiv_r_ui(temp, temp, 1000);
  ;;     } else {
  ;;       mpz_tdiv_r_ui(temp, quotient, 1000);
  ;;     }
  ;;     mpz_get_str(buf, 10, temp);
  ;;     strs.push_back(buf);
  ;;   }
  ;;
  ;;   bool printed = false;
  ;;
  ;;   for (std::list<string>::reverse_iterator i = strs.rbegin();
  ;;        i != strs.rend();
  ;;        i++) {
  ;;     if (printed) {
  ;;       out << (comm.has_flags(COMMODITY_STYLE_EUROPEAN) ? '.' : ',');
  ;;       out.width(3);
  ;;       out.fill('0');
  ;;     }
  ;;     out << *i;
  ;;
  ;;     printed = true;
  ;;   }
  ;; }
  ;;
  ;; if (quantity && precision) {
  ;;   std::ostringstream final;
  ;;   final.width(precision);
  ;;   final.fill('0');
  ;;   char * p = mpz_get_str(NULL, 10, rquotient);
  ;;   final << p;
  ;;   std::free(p);
  ;;
  ;;   const string& str(final.str());
  ;;   int i, len = str.length();
  ;;   const char * q = str.c_str();
  ;;   for (i = len; i > 0; i--)
  ;;     if (q[i - 1] != '0')
  ;;       break;
  ;;
  ;;   string ender;
  ;;   if (i == len)
  ;;     ender = str;
  ;;   else if (i < comm.precision())
  ;;     ender = string(str, 0, comm.precision());
  ;;   else
  ;;     ender = string(str, 0, i);
  ;;
  ;;   if (! ender.empty()) {
  ;;     if (omit_commodity)
  ;;       out << '.';
  ;;     else
  ;;       out << (comm.has_flags(COMMODITY_STYLE_EUROPEAN) ? ',' : '.');
  ;;     out << ender;
  ;;   }
  ;; }
  ;;
  ;; if (! omit_commodity && comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
  ;;   if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
  ;;     out << " ";
  ;;   comm.print(out);
  ;; }
  ;;
  ;; mpz_clear(quotient);
  ;; mpz_clear(rquotient);
  ;; mpz_clear(remainder);
  ;;
  ;; // If there are any annotations associated with this commodity,
  ;; // output them now.
  ;;
  ;; if (! omit_commodity && comm.annotated) {
  ;;   annotated_commodity_t& ann(static_cast<annotated_commodity_t&>(comm));
  ;;   assert(&*ann.details.price != this);
  ;;   ann.write_annotations(out);
  ;; }
  ;;
  ;; // Things are output to a string first, so that if anyone has
  ;; // specified a width or fill for _out, it will be applied to the
  ;; // entire amount string, and not just the first part.
  ;;
  ;; _out << out.str();
  (assert (or amount out omit-commodity full-precision)))

;; jww (2007-10-15): Add back this builtin commodity
;;  commodity->add_flags(COMMODITY_STYLE_NOMARKET | COMMODITY_STYLE_BUILTIN);
;;
;;  parse_conversion("1.0m", "60s");
;;  parse_conversion("1.0h", "60m");

(defun amount--resize (amount precision)
  ;; assert(prec < 256);
  ;;
  ;; if (! quantity || prec == quantity->prec)
  ;;   return;
  ;;
  ;; _dup();
  ;;
  ;; assert(prec > quantity->prec);
  ;; mpz_ui_pow_ui(divisor, 10, prec - quantity->prec);
  ;; mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  ;;
  ;; quantity->prec = prec;
  (assert (or amount precision)))

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
(defmethod add ((balance balance) (other balance))
  ;; for (amounts_map::const_iterator i = bal.amounts.begin();
  ;;      i != bal.amounts.end();
  ;;      i++)
  ;;   *this += i->second;
  ;; return *this;
  (assert (or balance other)))

(defmethod add ((balance balance) (other amount))
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

(defmethod subtract ((balance balance) (other balance))
  ;; for (amounts_map::const_iterator i = bal.amounts.begin();
  ;;      i != bal.amounts.end();
  ;;      i++)
  ;;   *this -= i->second;
  ;; return *this;
  (assert (or balance other)))

(defmethod subtract ((balance balance) (other amount))
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

(defmethod multiply ((balance balance) (other amount))
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

(defmethod divide ((balance balance) (other amount))
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

(defmethod smaller-units ((balance balance))
  ;; balance_t temp(*this);
  ;; temp.in_place_reduce();
  ;; return temp;
  (assert balance))

(defmethod smaller-units* ((balance balance))
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
