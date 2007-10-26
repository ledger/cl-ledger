;; -*- mode: lisp -*-

;; cambl.lisp - This is the Commoditized AMounts and BaLances library.

;;;_* Copyright (c) 2003-2007, John Wiegley.  All rights reserved.

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

;;;_* Commentary

;; This library aims to provide convenient access to commoditized math.  That
;; is, math involving numbers with units.  However, unlike scientific units,
;; this library does not allow the use of "compound" units.  If you divide 1kg
;; by 1m, you do not get "1 kg/m", but an error.  This is because the intended
;; use of this library is for situations that do not allow compounded units,
;; such as financial transactions, and the algorithms have been simplified
;; accordingly.  It also allows contextual information to be tracked during
;; commodity calculations -- the time of conversion from one type to another,
;; the rated value at the time of conversion.

;;;_ * Usage: Amounts

;; There are just a few main entry points to the CAMBL library for dealing
;; with amounts (which is mainly how you'll use it).  Here is a quick list,
;; following by a description in the context of a REPL session.  Note that
;; where the name contains value, it will work for integers, amounts, balances
;; and cost balances.  If it contains amount or balance, it only operates on
;; entities of that type.
;;
;;   amount[*]          ; create an amount from a string
;;   exact-amount       ; create an amount from a string which overrides
;;                      ; its commodity's display precision; this feature
;;                      ; "sticks" through any math operations
;;   parse-amount[*]    ; parse an amount from a string (alias for `amount')
;;   read-amount[*]     ; read an amount from a stream
;;   read-exact-amount  ; read an exact amount from a stream
;;   format-value       ; format a value to a string
;;   print-value        ; print a value to a stream
;;
;;   add[*]             ; perform math using values; the values are
;;   subtract[*]        ; changed as necessary to preserve information
;;   multiply[*]        ; (adding two amounts may result in a balance)
;;   divide[*]          ; the * versions change the first argument.
;;
;;   value-zerop        ; would the value display as zero?
;;   value-zerop*       ; is the value truly zero?
;;
;;   value-minusp       ; would the amount display as a negative amount?
;;   value-minusp*      ; is the amount truly negative?
;;   value-plusp        ; would it display as an amount greater than zero?
;;   value-plusp*       ; is it truly greater than zero?
;;
;;   value=, value/=    ; compare two values
;;   value<, value<=    ; compare values (not meaningful for all values,
;;   value>, value>=    ; such as balances)
;;
;;   value-equal        ; compare two values for exact match
;;   value-equalp       ; compare two values only after rounding to what
;;                      ; would be shown to the user (approximate match)
;;                      ; -- this is the same as value=
;;   value-not-equal    ; compare two values for no exact match
;;   value-not-equalp   ; compare two values after commodity rounding
;;
;;   value-lessp        ; same as value<
;;   value-lessp*       ; compare if a < b exactly, with full precision
;;   value-lesseqp      ; same as value<=
;;   value-lesseqp*     ; exact version of value<=
;;   value-greaterp     ; same as value>
;;   value-greaterp*    ; exact version of value>
;;   value-greatereqp   ; same as value>=
;;   value-greatereqp*  ; exact version of value>=
;;
;;   amount-precision   ; return the internal precision of an amount
;;   display-precision  ; return the "display" precision for an amount
;;                      ; or a commodity
;;
;;   *european-style*   ; set this to T if you want to see 1.000,00 style
;;                      ; numbers printed instead of 1,000.00 (US) style.
;;
;; Most all interactions with CAMBL begins with the creation of an amount.
;; This is easiest done from a string, but can also be read from a stream:
;;
;;   (cambl:amount "$100.00") =>
;;     #<CAMBL:AMOUNT "$100.00" :KEEP-PRECISION-P NIL>
;;
;;   (with-input-from-string (in "$100.00"))
;;      (cambl:read-amount in)) =>
;;     #<CAMBL:AMOUNT "$100.00" :KEEP-PRECISION-P NIL>
;;
;; When you parse an amount using one of these two functions, CAMBL creates a
;; COMMODITY class, whose symbol name is "$".  This class remembers certain
;; details about how you used the commodity, such as the input precision, and
;; the format of the commodity symbol.  Some of these details can be inspected
;; by looking at the amount's commodity directly:
;;
;;   (cambl:amount-commodity (cambl:amount "$100.00")) =>
;;     #<CAMBL::COMMODITY #S(CAMBL::COMMODITY-SYMBOL
;;                           :NAME $
;;                           :NEEDS-QUOTING-P NIL
;;                           :PREFIXED-P T
;;                           :CONNECTED-P T)
;;         :THOUSAND-MARKS-P NIL :DISPLAY-PRECISION 2>
;;
;; Here you see that the commodity for $100.00 references a COMMODITY-SYMBOL,
;; which knows that: 1) the commodity should be prefixed to its amount; and 2)
;; it's connected to the amount.  The commodity was used without any "thousand
;; marks" (i.e., $1000.00 vs $1,000.00), and it had a maximum display
;; precision of two observed so far.  If we print sach an amount, we'll now
;; see the same style of output as was input:
;;
;;   (cambl:format-value (cambl:amount "$100.00")) => "$100.00"
;;   (cambl:print-value (cambl:amount "$100.00"))  => NIL
;;     $100.00
;;
;; So CAMBL watched how you used the "$" commodity, and will now report back
;; all dollar figures in the same fashion.  Even though there are no cents in
;; the amounts above, CAMBL realizes you want to record a full two digits of
;; precision.
;;
;;   (cambl:amount-precision (cambl:amount "$100.00")) => 2
;;
;; CAMBL always observes greater precision, but never observes less precision.
;; So if you parse $100.00 and then $100, both values are printed as $100.00.
;;
;; There are three variantions on the parsing and reading functions, which
;; also create amounts but which change how percision is handled.  The
;; differences are:
;;
;;   (cambl:amount "$100.00") =>
;;     #<CAMBL:AMOUNT "$100.00" :KEEP-PRECISION-P NIL>
;;
;;     a. amount has an internal precision of 2
;;     b. commodity $ has a display precision of 2 (if no other
;;        amount using a higher precision was observed so far)
;;     c. when printing, amount uses the commodity's precision
;;
;;       (cambl:format-value *)                      => "$100.00"
;;       (cambl:format-value ** :full-precision-p t) => "$100.00"
;;
;;   (cambl:amount* "$100.0000") =>
;;     #<CAMBL:AMOUNT "$100.0000" :KEEP-PRECISION-P NIL>
;;
;;     a. amount has an internal precision of 4
;;     b. commodity $ still has a display precision of 2 (from above)
;;     c. when printing, amount uses the commodity's precision
;;
;;       (cambl:format-value *)                      => "$100.00"
;;       (cambl:format-value ** :full-precision-p t) => "$100.0000"
;;
;;   (cambl:exact-amount "$100.0000") =>
;;     #<CAMBL:AMOUNT "$100.0000" :KEEP-PRECISION-P T>
;;
;;     a. amount has an internal precision of 4
;;     b. commodity $ still has a display precision of 2 (from above)
;;     c. when printing, amount uses its internal precision
;;
;;       (cambl:format-value *)                      => "$100.0000"
;;       (cambl:format-value ** :full-precision-p t) => "$100.0000"
;;
;; There are similar variants for the stream reading functions:
;;
;;   read-amount
;;   read-amount*
;;   read-exact-amount
;;
;; NOTE: The KEEP-PRECISION-P property of an amount carries through any math
;; operations involving that amount, so that the final result always displays
;; using its own internal percision also.
;;
;; The point of all this is that amounts are displayed as the user expects
;; them to be, but internally never lose information.  In fact, if you divide
;; two high internal precision amounts together, you'll get a new amount with
;; a very high internal precision, but which still displays as expected:
;;
;;   (setq *tmp* (cambl:divide  (cambl:amount "$100.00")
;;                              (cambl:amount "50000000")))
;;
;;   (cambl:format-value *tmp* :full-precision-p t) =>
;;     "$0.000002000000000"
;;   (cambl:format-value *tmp*) => "$0.00"
;;
;; You'll notice here that the amount displayed is not $0.00000200000000002.
;; This is because CAMBL does not try to capture every digit resulting from a
;; division; rather, it keeps six more digits of precision than is strictly
;; necessary so that even after millions of calculations, not a penny is lost.
;; If you find this is not enough slack for your calculations, you can set
;; CAMBL:*EXTRA-PRECISION* to a higher or lower value.

;;;_ * Usage: Commodities

;; CAMBL offers several methods for accessing the commodity information
;; relating to amounts:
;;
;;   amount-commodity          ; the COMMODITY referenced by an amount
;;   display-precision         ; display precision of an AMOUNT or COMMODITY
;;   commodity-serial-number   ; the unique serial number of each COMMODITY
;;   commodity-qualified-name  ; the name used print a COMMODITY
;;   commodity-annotated-p     ; does the commodity have an annotation?

;;;_* Todo
;;
;; - balances: unary math (negate, abs)
;; - balances: binary math (add, subtract, multiply, divide)
;; - balances: printing
;; - cost balances: everything
;; - commodity stripping
;; - commodity conversions
;; - downloading quotes
;;
;; - Create a function for calculating a conversion, which will
;;   automatically preserve annotation details based on context:
;;     (amount-exchange "100 DM" "$100.00" &optional datetime "Note")
;;   This will "exchange" 100 DM for $100.00, at the given datetime with
;;   the given note.  It will be called `exchange-commodity'.

;;;_* Package

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defpackage :cambl
  (:use :cl :rbt)
  (:export *european-style*

	   amount
	   amount*
	   parse-amount
	   parse-amount*
	   exact-amount
	   read-amount
	   read-amount*
	   read-exact-amount
	   float-to-amount
	   integer-to-amount

	   copy-amount
	   copy-balance
	   copy-value

	   value-zerop
	   value-zerop*
	   value-minusp
	   value-minusp*
	   value-plusp
	   value-plusp*

	   compare
	   compare*
	   sign
	   sign*

	   value-equal
	   value-equalp
	   value-not-equal
	   value-not-equalp
	   value=
	   value/=

	   value-lessp
	   value-lessp*
	   value-lesseqp
	   value-lesseqp*
	   value<
	   value<=

	   value-greaterp
	   value-greaterp*
	   value-greatereqp
	   value-greatereqp*
	   value>
	   value>=

	   value-abs
	   value-round
	   value-round*

	   negate*
	   negate
	   add
	   add*
	   subtract
	   subtract*
	   multiply
	   multiply*
	   divide
	   divide*

	   print-value
	   format-value
	   quantity-string

	   smallest-units
	   smaller-units
	   larger-units
	   convert-to-units

	   exchange-commodity
	   purchase-commodity
	   sell-commodity

	   amount-commodity
	   commodity-name
	   amount-precision
	   display-precision

	   amount-error

	   commodity
	   commodity-annotation
	   annotated-commodity

	   commodity-equal
	   commodity-equalp

	   market-value

	   commodity-serial-number
	   commodity-qualified-name
	   commodity-thousand-marks-p
	   commodity-lessp

	   commodity-annotated-p
	   commodity-annotation
	   commodity-annotation-equal
	   annotate-commodity
	   strip-annotations

	   *default-commodity-pool*
	   make-commodity-pool
	   reset-commodity-pool
	   find-commodity
	   find-annotated-commodity

	   commodity-error))

(in-package :cambl)

;;;_* Types

(deftype datetime ()
  'integer)

;;;_ - COMMODITY-SYMBOL

(defstruct commodity-symbol
  (name "" :type string)
  (needs-quoting-p nil :type boolean)
  (prefixed-p nil :type boolean)
  (connected-p nil :type boolean))

;;;_ - BASIC-COMMODITY

(defstruct (basic-commodity (:conc-name get-))
  (symbol nil :type commodity-symbol)
  (description nil :type (or string null))
  (comment nil :type (or string null))
  (smaller-unit-equivalence nil)	; (:type amount)
  (larger-unit-equivalence nil)		; (:type amount)
  (thousand-marks-p nil :type boolean)
  (no-market-price-p nil :type boolean)
  (builtin-p nil :type boolean)
  (display-precision 0 :type fixnum)
  (price-history nil)
  (last-lookup nil :type (or datetime null)))

(defstruct pricing-entry
  (moment nil :type datetime)
  (price nil))				; (:type amount)

;;;_ - COMMODITY-POOL

(defstruct commodity-pool
  (by-name-map (make-hash-table :test 'equal) :type hash-table)
  (by-serial-list '((0 . nil)) :type list)
  (default-commodity nil))

(defparameter *default-commodity-pool* (make-commodity-pool))

;;;_ + COMMODITY

;; Commodities are references to basic commodities, which store the common
;; details.  This is so the commodities USD and $ can both refer to the same
;; underlying kind.

(defclass commodity ()
  ((basic-commodity :accessor get-basic-commodity :initarg :basic-commodity
		    :type basic-commodity)
   (commodity-pool :accessor get-commodity-pool :initarg :commodity-pool
		   :type commodity-pool)
   (serial-number :accessor commodity-serial-number :initarg :serial-number
		  :type fixnum)
   (qualified-name :accessor commodity-qualified-name :initarg :qualified-name
		   :type (or string null))
   ;; This is set automatically by initialize-instance whenever an
   ;; annotated-commodity is created.
   (annotated-p :accessor get-annotated-p :type boolean)))

(defmethod print-object ((commodity commodity) stream)
  (print-unreadable-object (commodity stream :type t)
    (let ((base (get-basic-commodity commodity)))
      (princ (get-symbol base) stream)
      (format stream "~%    :THOUSAND-MARKS-P ~S :DISPLAY-PRECISION ~D"
	      (get-thousand-marks-p base) (get-display-precision base)))))

;;;_ + COMMODITY-ANNOTATION

(defstruct (commodity-annotation
	     (:conc-name annotation-))
  (price nil) ;; (:type (or amount null))
  (date nil :type (or datetime null))
  (tag nil :type (or string null)))

;;;_ + ANNOTATED-COMMODITY

(defclass annotated-commodity (commodity)
  ((referent-commodity :accessor get-referent-commodity
		       :initarg :referent-commodity :type commodity)
   (annotation :accessor get-annotation :initarg :annotation
	       ;; :type commodity-annotation
	       )))

(defmethod initialize-instance :after
    ((annotated-commodity annotated-commodity) &key)
  (setf (get-annotated-p annotated-commodity) t))

;;;_ + AMOUNT

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

(defstruct (amount (:print-function print-amount))
  (commodity  nil :type (or commodity null))
  (quantity 0 :type integer)
  (precision 0 :type fixnum)
  (keep-precision-p nil :type boolean)
  ;;value-origins-list
  ;;(keep-base :allocation :class :initform nil :type boolean)
  ;;(keep-price :allocation :class :initform nil :type boolean)
  ;;(keep-date :allocation :class :initform nil :type boolean)
  ;;(keep-tag :allocation :class :initform nil :type boolean)
  )

;;;_ + BALANCE

(defclass balance ()
  ((amounts-map :accessor get-amounts-map :initform nil)))

;;;_ + COST-BALANCE

(defclass cost-balance (balance)
  ((costs-map  :accessor get-costs-map :initform nil)))

;;;_* Generics

;;;_ + Public generics

;; - If the argument says item, this means:
;;   amount, commodity, annotated-commodity, null
;; - If the argument says any-item, this means:
;;   amount, balance, cost-balance, commodity, annotated-commodity, null
;; - If it says value, this means:
;;   amount, balance, cost-balance

(defgeneric copy-value (value))

(defgeneric value-zerop (value))
(defgeneric value-zerop* (value))	; is it *really* zerop?

(defgeneric value-equal (left right))
(defgeneric value-equalp (left right))
(defgeneric value-not-equal (left right))
(defgeneric value-not-equalp (left right))
(defgeneric value= (left right))
(defgeneric value/= (left right))

(defgeneric value-abs (value))
(defgeneric value-round (value &optional precision))
(defgeneric value-round* (value &optional precision))

(defgeneric negate* (value))
(defgeneric negate (value))
(defgeneric add (value-a value-b))
(defgeneric add* (value-a value-b))
(defgeneric subtract (value-a value-b))
(defgeneric subtract* (value-a value-b))
(defgeneric multiply (value-a value-b))
(defgeneric multiply* (value-a value-b))
(defgeneric divide (value-a value-b))
(defgeneric divide* (value-a value-b))

(defgeneric optimize-value (value))

(defgeneric print-value (value &key output-stream omit-commodity-p
			       full-precision-p width latter-width))
(defgeneric format-value (value &key omit-commodity-p full-precision-p
				width latter-width))

(defgeneric amount-in-balance (balance commodity))

(defgeneric smallest-units (value))
(defgeneric smaller-units (value))
(defgeneric larger-units (value))
(defgeneric convert-to-units (value commodity))

(defgeneric commodity-equal (item-a item-b))
(defgeneric commodity-equalp (item-a item-b)) ; ignores annotation

(defgeneric commodity-name (item))
(defgeneric display-precision (item))

(defgeneric market-value (any-item &optional datetime))

(defgeneric commodity-annotated-p (item))
(defgeneric commodity-annotation (item))
(defgeneric commodity-annotation-equal (item item))
(defgeneric annotate-commodity (item annotation))
(defgeneric strip-annotations (any-item &key keep-price keep-date keep-tag))

;;;_ - Private generics

(defgeneric commodity-base (commodity))
(defgeneric commodity-symbol (commodity))
(defgeneric commodity-precision (commodity))
(defgeneric commodity-thousand-marks-p (commodity))
(defgeneric commodity-description (item))
(defgeneric commodity-no-market-price-p (commodity))
(defgeneric commodity-builtin-p (commodity))

(defgeneric commodity-annotation-empty-p (item))

;;;_* Object printing functions

(defun print-amount (amount stream depth)
  (declare (ignore depth))
  (print-unreadable-object (amount stream :type t)
    (format stream "~S :KEEP-PRECISION-P ~S"
	    (format-value amount :full-precision-p t)
	    (amount-keep-precision-p amount))))

;;;_* Constants

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

;;;_* Global Variables

(defvar *european-style* nil
  "If set to T, amounts will be printed as 1.000,00.
  The default is US style, which is 1,000.00.  Note that thousand markers are
  only used if the commodity's own `thousand-marks-p' accessor returns T.")

(defvar *extra-precision* 6
  "Specify the amount of \"extra\" to be added during amount division.")

(defvar *keep-annotation-prices* t)
(defvar *keep-annotation-dates* t)
(defvar *keep-annotation-tags* t)

(defvar *get-price-quote-function* nil)

;; jww (2007-10-15): Add back this builtin commodity
;;  commodity->add_flags(COMMODITY_STYLE_NOMARKET | COMMODITY_STYLE_BUILTIN);
;;
;;  parse_conversion("1.0m", "60s");
;;  parse_conversion("1.0h", "60m");

;;;_* Functions

;;;_ * AMOUNT, BALANCE and COST-BALANCE

;;;_  + Error class

(define-condition amount-error (error) 
  ((description :reader error-description :initarg :msg))
  (:report (lambda (condition stream)
	     (format stream "~S" (error-description condition)))))

;;;_  + Create AMOUNT objects from strings

(defun amount (text &key (reduce-to-smallest-units-p t)
	       (pool *default-commodity-pool*))
  (declare (type string text))
  (with-input-from-string (in text)
    (read-amount in :observe-properties-p t
		 :reduce-to-smallest-units-p reduce-to-smallest-units-p
		 :pool pool)))

(defun amount* (text &key (reduce-to-smallest-units-p t)
		(pool *default-commodity-pool*))
  (declare (type string text))
  (with-input-from-string (in text)
    (the amount
      (read-amount in :observe-properties-p nil
		   :reduce-to-smallest-units-p reduce-to-smallest-units-p
		   :pool pool))))

(declaim (ftype function parse-amount))
(setf (fdefinition 'parse-amount) (fdefinition 'amount))

(declaim (ftype function parse-amount*))
(setf (fdefinition 'parse-amount*) (fdefinition 'amount*))

(defun exact-amount (text &key (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (let ((amount
	 (amount* text :reduce-to-smallest-units-p reduce-to-smallest-units-p
		       :pool pool)))
    (setf (amount-keep-precision-p amount) t)
    amount))

;;;_  + Read AMOUNT objects from streams

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
  (declare (type stream in))

  (let (symbol quantity details negative-p
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
		 (not (cl:zerop (length (commodity-symbol-name symbol)))))
	    (if details
		(find-annotated-commodity symbol details :pool pool
					  :create-if-not-exists-p t)
		(find-commodity symbol :pool pool :create-if-not-exists-p t))
	    (values nil nil))

      ;; Determine the precision of the amount, based on the usage of
      ;; comma or period.
      (setq amount (make-amount :commodity commodity))

      (let ((last-comma (position #\, quantity :from-end t))
	    (last-period (position #\. quantity :from-end t)))
	(if (or (and *european-style* last-period)
		(and (not *european-style*) last-comma))
	    (setq thousand-marks-p t))
	(cond ((and last-comma last-period)
	       (setf (amount-precision amount)
		     (- (length quantity) (if (> last-comma last-period)
					      last-comma last-period) 1)))
	      ((and last-comma *european-style*)
	       (setf (amount-precision amount)
		     (- (length quantity) last-comma 1)))
	      ((and last-period (not *european-style*))
	       (setf (amount-precision amount)
		     (- (length quantity) last-period 1)))
	      (t
	       (setf (amount-precision amount) 0))))

      (setf (amount-quantity amount)
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
	    (setf (get-thousand-marks-p (commodity-base commodity))
		  thousand-marks-p))

	(let ((precision (amount-precision amount)))
	  (if (> precision (display-precision commodity))
	      (setf (get-display-precision (commodity-base commodity))
		    precision)))))

    (if negative-p
	(negate* amount))

    (if reduce-to-smallest-units-p
	(smallest-units amount))

    (the amount amount)))

(defun read-amount* (in &key (reduce-to-smallest-units-p t)
		     (pool *default-commodity-pool*))
  (read-amount in :observe-properties-p nil
	       :reduce-to-smallest-units-p reduce-to-smallest-units-p
	       :pool pool))

(defun read-exact-amount (text &key (reduce-to-smallest-units-p t)
			  (pool *default-commodity-pool*))
  (let ((amount
	 (read-amount* text :reduce-to-smallest-units-p reduce-to-smallest-units-p
			    :pool pool)))
    (setf (amount-keep-precision-p amount) t)
    amount))

;;;_  + Convert other values to and from AMOUNT

(declaim (inline float-to-amount))

(defun float-to-amount (value)
  (declare (type float value))
  (the amount
    (parse-amount* (format nil "~F" value))))

(declaim (inline integer-to-amount))

(defun integer-to-amount (value)
  (declare (type (or integer fixnum) value))
  (the amount
    (make-amount :quantity value :precision 0)))

(defun amount-to-integer (amount &key (dont-check-p t))
  (declare (type amount amount))
  (declare (type boolean dont-check-p))
  (multiple-value-bind (quotient remainder)
      (truncate (amount-quantity amount)
		(expt 10 (amount-precision amount)))
    (if (and (not dont-check-p) remainder)
	(error 'amount-error :msg
	       "Conversion of amount to integer loses precision"))
    quotient))

;;;_  + Copiers

(defmethod copy-value ((amount amount))
  (copy-amount amount))

(defun copy-balance (balance)
  balance)

(defmethod copy-value ((balance balance))
  (copy-balance balance))

(defmethod copy-value ((cost-balance cost-balance))
  (copy-balance cost-balance))

;;;_  + Unary truth tests

(defmethod value-zerop ((amount amount))
  (cl:zerop (amount-quantity (value-round amount))))
(defmethod value-zerop ((balance balance))
  (the boolean
    (block nil
      (maphash #'(lambda (key value)
		   (declare (ignore key))
		   (unless (value-zerop value)
		     (return nil)))
	       (get-amounts-map balance))
      t)))

(defmethod value-zerop* ((amount amount))
  (cl:zerop (amount-quantity amount)))
(defmethod value-zerop* ((balance balance))
  (the boolean
    (block nil
      (maphash #'(lambda (key value)
		   (declare (ignore key))
		   (unless (value-zerop* value)
		     (return nil)))
	       (get-amounts-map balance))
      t)))

(declaim (inline value-minusp value-minusp*))

(defun value-minusp (amount)
  (cl:minusp (amount-quantity (value-round amount))))
(defun value-minusp* (amount)
  (cl:minusp (amount-quantity amount)))

(declaim (inline value-plusp value-plusp*))

(defun value-plusp (amount)
  (cl:plusp (amount-quantity (value-round amount))))
(defun value-plusp* (amount)
  (cl:plusp (amount-quantity amount)))

;;;_  + AMOUNT comparison (return sort order of value)

(defun verify-amounts (left right capitalized-gerund)
  (declare (type amount left))
  (declare (type amount right))

  (unless (commodity-equal (amount-commodity left)
			   (amount-commodity right))
    (error 'amount-error :msg
	   (format nil "~A amounts with different commodities: ~A != ~A"
		   capitalized-gerund
		   (commodity-name left)
		   (commodity-name right)))))

(defun compare (left right)
  (verify-amounts left right "Comparing")
  (let ((left-rounded (value-round left))
	(right-rounded (value-round right)))
    (compare* left-rounded right-rounded)))

(defun compare* (left right)
  (verify-amounts left right "Exactly comparing")
  (cond ((= (amount-precision left)
	    (amount-precision right))
	 (- (amount-quantity left)
	    (amount-quantity right)))
	((< (amount-precision left)
	    (amount-precision right))
	 (let ((tmp (copy-amount left)))
	   (amount--resize tmp (amount-precision right))
	   (- (amount-quantity tmp)
	      (amount-quantity right))))
	(t
	 (let ((tmp (copy-amount right)))
	   (amount--resize tmp (amount-precision left))
	   (- (amount-quantity left)
	      (amount-quantity tmp))))))

(defun sign (amount)
  "Return -1, 0 or 1 depending on the sign of AMOUNT."
  (sign* (value-round amount)))

(defun sign* (amount)
  "Return -1, 0 or 1 depending on the sign of AMOUNT."
  (assert amount)
  (let ((quantity (amount-quantity amount)))
    (if (cl:minusp quantity)
	-1
	(if (cl:plusp quantity)
	    1
	    0))))

;;;_  + Equality tests

(defun amount-balance-equal (left right test-func)
  "Compare with the amount LEFT equals the balance RIGHT."
  (the boolean
    (let ((amounts-map (get-amounts-map right)))
      (if (value-zerop* left)
	  (or (null amounts-map)
	      (cl:zerop (hash-table-count amounts-map)))
	  (if (and amounts-map
		   (= 1 (hash-table-count amounts-map)))
	      (let ((amount-value (gethash (amount-commodity left)
					   amounts-map)))
		(if amount-value
		    (funcall test-func amount-value left))))))))

(defun balance-equal (left right test-func)
  (let ((left-amounts-map (get-amounts-map left))
	(right-amounts-map (get-amounts-map right)))
    (the boolean
      (when (= (hash-table-count left-amounts-map)
	       (hash-table-count right-amounts-map))
	(block nil
	  (maphash
	   #'(lambda (key value)
	       (let ((right-value
		      (gethash key right-amounts-map)))
		 (unless right-value
		   (return nil))
		 (unless (funcall test-func value right-value)
		   (return nil))))
	   left-amounts-map)
	  t)))))

(defmethod value-equal ((left amount) (right amount))
  (cl:zerop (compare* left right)))
(defmethod value-equal ((left amount) (right balance))
  (amount-balance-equal left right #'value-equal))
(defmethod value-equal ((left balance) (right amount))
  (amount-balance-equal right left #'value-equal))
(defmethod value-equal ((left balance) (right balance))
  (balance-equal left right #'value-equal))

(defmethod value-equalp ((left amount) (right amount))
  (cl:zerop (compare left right)))
(defmethod value-equalp ((left amount) (right balance))
  (amount-balance-equal left right #'value-equalp))
(defmethod value-equalp ((left balance) (right amount))
  (amount-balance-equal right left #'value-equalp))
(defmethod value-equalp ((left balance) (right balance))
  (balance-equal left right #'value-equalp))

(defmethod value= ((left amount) (right amount))
  (value-equalp left right))
(defmethod value/= ((left amount) (right amount))
  (not (value-equalp left right)))

;;;_  + Comparison tests

(declaim (inline value-lessp value-lessp*))

(defun value-lessp (left right)
  (declare (type amount left))
  (declare (type amount right))
  (cl:minusp (compare left right)))

(defun value-lessp* (left right)
  (declare (type amount left))
  (declare (type amount right))
  (cl:minusp (compare* left right)))

(declaim (inline value-lesseqp value-lesseqp*))

(defun value-lesseqp (left right)
  (declare (type amount left))
  (declare (type amount right))
  (<= (compare left right) 0))

(defun value-lesseqp* (left right)
  (declare (type amount left))
  (declare (type amount right))
  (<= (compare* left right) 0))

(declaim (inline value< value<=))

(defun value< (left right)
  (declare (type amount left))
  (declare (type amount right))
  (cl:minusp (compare left right)))

(defun value<= (left right)
  (declare (type amount left))
  (declare (type amount right))
  (<= (compare left right) 0))

(declaim (inline value-greaterp value-greaterp*))

(defun value-greaterp (left right)
  (declare (type amount left))
  (declare (type amount right))
  (cl:plusp (compare left right)))

(defun value-greaterp* (left right)
  (declare (type amount left))
  (declare (type amount right))
  (cl:plusp (compare* left right)))

(declaim (inline value-greatereqp value-greatereqp*))

(defun value-greatereqp (left right)
  (declare (type amount left))
  (declare (type amount right))
  (>= (compare left right) 0))

(defun value-greatereqp* (left right)
  (declare (type amount left))
  (declare (type amount right))
  (>= (compare* left right) 0))

(declaim (inline value> value>=))

(defun value> (left right)
  (declare (type amount left))
  (declare (type amount right))
  (cl:plusp (compare left right)))

(defun value>= (left right)
  (declare (type amount left))
  (declare (type amount right))
  (>= (compare left right) 0))

;;;_  + Unary math operators

(defmethod value-abs ((amount amount))
  (assert amount)
  (if (cl:minusp (amount-quantity amount))
      (negate amount)
      amount))

(defmethod value-abs ((balance balance))
  (let ((value-balance (make-instance 'balance)))
    (maphash #'(lambda (commodity amount)
		 (declare (ignore commodity))
		 (add* value-balance
		       (value-abs amount)))
	     (get-amounts-map balance))
    value-balance))

(defmethod value-round ((amount amount) &optional precision)
  (let ((tmp (copy-amount amount)))
    (value-round* tmp precision)))

(defmethod value-round* ((amount amount) &optional precision)
  "Round the given AMOUNT to the stated internal PRECISION.
  If PRECISION is less than the current internal precision, data will
  be lost.  If it is greater, the integer value of the amount is
  increased until the target precision is reached."
  (unless precision
    (let ((commodity (amount-commodity amount)))
      (if commodity
	  (setq precision (display-precision commodity))
	  (setq precision 0))))

  (let ((internal-precision (amount-precision amount)))
    (cond ((< precision internal-precision)
	   (setf (amount-quantity amount)
		 (nth-value 0 (truncate (amount-quantity amount)
					(expt 10 (- internal-precision
						    precision)))))
	   (setf (amount-precision amount) precision))
	  ((> precision internal-precision)
	   (setf (amount-quantity amount)
		 (* (amount-quantity amount)
		    (expt 10 (- precision internal-precision))))
	   (setf (amount-precision amount) precision))))
  amount)

(defmethod negate ((amount amount))
  (assert amount)
  (let ((tmp (copy-amount amount)))
    (negate* tmp)))

(defmethod negate ((balance balance))
  (let ((tmp (copy-balance balance)))
    ;; (negate* tmp)
    (assert tmp)
    ))

(defmethod negate* ((amount amount))
  (assert amount)
  (setf (amount-quantity amount)
	(- (amount-quantity amount)))
  amount)

(defmethod negate* ((balance balance))
  (let ((value-balance (make-instance 'balance)))
    (maphash #'(lambda (commodity amount)
		 (declare (ignore commodity))
		 (add* value-balance
		       (negate amount)))
	     (get-amounts-map balance))
    value-balance))

;;;_  + Binary math operators

;;;_   : Addition

(defun amount--resize (amount precision)
  (assert (< precision 256))
  (unless (or (not (amount-quantity amount))
	      (= precision (amount-precision amount)))
    (assert (> precision (amount-precision amount)))
    (setf (amount-quantity amount)
	  (* (amount-quantity amount)
	     (expt 10 (- precision
			 (amount-precision amount)))))
    (setf (amount-precision amount) precision)))

(defmethod add ((left integer) (right integer))
  (+ left right))
(defmethod add* ((left integer) (right integer))
  (+ left right))
(defmethod add ((left amount) (right integer))
  (add* (copy-amount left) (integer-to-amount right)))
(defmethod add* ((left amount) (right integer))
  (add* left (integer-to-amount right)))
(defmethod add ((left integer) (right amount))
  (add* (integer-to-amount left) right))
(defmethod add* ((left integer) (right amount))
  (add* (integer-to-amount left) right))

(defmethod add ((left amount) (right amount))
  (add* (copy-amount left) right))
(defmethod add* ((left amount) (right amount))
  (verify-amounts left right "Adding")
  (let ((left-quantity (amount-quantity left))
	(right-quantity (amount-quantity right)))
    (cond ((= (amount-precision left)
	      (amount-precision right))
	   (setf (amount-quantity left)
		 (+ left-quantity right-quantity)))
	  ((< (amount-precision left)
	      (amount-precision right))
	   (amount--resize left (amount-precision right))
	   (setf (amount-quantity left)
		 (+ left-quantity right-quantity)))
	  (t
	   (let ((tmp (copy-amount right)))
	     (amount--resize tmp (amount-precision left))
	     (setf (amount-quantity left)
		   (+ left-quantity right-quantity))))))
  left)

(defmethod add ((left amount) (right balance))
  (add right left))
(defmethod add* ((left amount) (right balance))
  (add right left))

(defmethod add ((left balance) (right amount))
  (add* (copy-balance left) right))
(defmethod add* ((left balance) (right amount))
  (unless (value-zerop* right)
    (unless (get-amounts-map left)
      (setf (get-amounts-map left) (make-hash-table)))
    (let* ((amounts-map (get-amounts-map left))
	   (current-amount (gethash (amount-commodity right)
				    amounts-map)))
      (if current-amount
	  (add* current-amount right)	; modifies current-amount directly
	  (setf (gethash (amount-commodity right) amounts-map)
		right))))
  left)


(defmethod add ((left balance) (right balance))
  (add* (copy-balance left) right))
(defmethod add* ((left balance) (right balance))
  (maphash #'(lambda (commodity amount)
	       (declare (ignore commodity))
	       (add* left amount))
	   (get-amounts-map right)))

;;;_   : Subtraction

(defmethod subtract ((left integer) (right integer))
  (- left right))
(defmethod subtract* ((left integer) (right integer))
  (- left right))
(defmethod subtract ((left amount) (right integer))
  (subtract* (copy-amount left) (integer-to-amount right)))
(defmethod subtract* ((left amount) (right integer))
  (subtract* left (integer-to-amount right)))
(defmethod subtract ((left integer) (right amount))
  (subtract* (integer-to-amount left) right))
(defmethod subtract* ((left integer) (right amount))
  (subtract* (integer-to-amount left) right))

(defmethod subtract ((left amount) (right amount))
  (subtract* (copy-amount left) right))
(defmethod subtract* ((left amount) (right amount))
  (verify-amounts left right "Subtracting")
  (the amount
    (let ((left-quantity (amount-quantity left))
	  (right-quantity (amount-quantity right)))
      (cond ((= (amount-precision left)
		(amount-precision right))
	     (setf (amount-quantity left)
		   (- left-quantity right-quantity)))
	    ((< (amount-precision left)
		(amount-precision right))
	     (amount--resize left (amount-precision right))
	     (setf (amount-quantity left)
		   (- left-quantity right-quantity)))
	    (t
	     (let ((tmp (copy-amount right)))
	       (amount--resize tmp (amount-precision left))
	       (setf (amount-quantity left)
		     (- left-quantity right-quantity)))))
      left)))

(defmethod subtract ((left amount) (right balance)))
(defmethod subtract* ((left amount) (right balance))
  ;; jww (2007-10-22): NYI
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
  )

(defmethod subtract ((left balance) (right amount)))
(defmethod subtract* ((left balance) (right amount))
  ;; jww (2007-10-22): NYI
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
  )

(defmethod subtract ((left balance) (right balance)))
(defmethod subtract* ((left balance) (right balance))
  ;; jww (2007-10-22): NYI
  ;; for (amounts_map::const_iterator i = bal.amounts.begin();
  ;;      i != bal.amounts.end();
  ;;      i++)
  ;;   *this -= i->second;
  ;; return *this;
  )

;;;_   : Multiplication

(defmethod multiply ((left integer) (right integer))
  (* left right))
(defmethod multiply* ((left integer) (right integer))
  (* left right))
(defmethod multiply ((left amount) (right integer))
  (multiply* (copy-amount left) (integer-to-amount right)))
(defmethod multiply* ((left amount) (right integer))
  (multiply* left (integer-to-amount right)))
(defmethod multiply ((left integer) (right amount))
  (multiply* (integer-to-amount left) right))
(defmethod multiply* ((left integer) (right amount))
  (multiply* (integer-to-amount left) right))

(defun set-amount-commodity-and-round* (left right)
  (declare (type amount left))
  (declare (type amount right))

  (let ((commodity (amount-commodity left)))
    (unless commodity
      (setf (amount-commodity left)
	    (setq commodity (amount-commodity right))))

    ;; If this amount has a commodity, and we're not dealing with plain
    ;; numbers, or internal numbers (which keep full precision at all
    ;; times), then round the number to within the commodity's precision
    ;; plus six places.
    (when (and commodity (not (amount-keep-precision-p left)))
      (let ((commodity-precision (display-precision commodity)))
	(when (> (amount-precision left)
		 (+ *extra-precision* commodity-precision))
	  (setf (amount-quantity left)
		(cl:round (amount-quantity left)
			  (expt 10 (- (amount-precision left)
				      (+ *extra-precision* commodity-precision)))))
	  (setf (amount-precision left)
		(+ *extra-precision* commodity-precision))))))
  left)

(defmethod multiply ((left amount) (right amount))
  (let ((tmp (copy-amount left)))
    (multiply* tmp right)))
(defmethod multiply* ((left amount) (right amount))
  (setf (amount-quantity left)
	(* (amount-quantity left)
	   (amount-quantity right)))
  (setf (amount-precision left)
	(+ (amount-precision left)
	   (amount-precision right)))

  (set-amount-commodity-and-round* left right))

(defmethod multiply ((left balance) (right amount)))
(defmethod multiply* ((left balance) (right amount))
  ;; jww (2007-10-22): NYI
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
  )

(defmethod multiply ((left amount) (right balance))
  (error 'amount-error :msg "Cannot multiply an amount by a balance"))
(defmethod multiply* ((left amount) (right balance))
  (error 'amount-error :msg "Cannot multiply an amount by a balance"))

(defmethod multiply ((left balance) (right balance))
  (error 'amount-error :msg "Cannot multiply a balance by another balance"))
(defmethod multiply* ((left balance) (right balance))
  (error 'amount-error :msg "Cannot multiply a balance by another balance"))

;;;_   : Division

(defmethod divide ((left integer) (right integer))
  (/ left right))
(defmethod divide* ((left integer) (right integer))
  (/ left right))
(defmethod divide ((left amount) (right integer))
  (divide* (copy-amount left) (integer-to-amount right)))
(defmethod divide* ((left amount) (right integer))
  (divide* left (integer-to-amount right)))
(defmethod divide ((left integer) (right amount))
  (divide* (integer-to-amount left) right))
(defmethod divide* ((left integer) (right amount))
  (divide* (integer-to-amount left) right))

(defmethod divide ((left amount) (right amount))
  (let ((tmp (copy-amount left)))
    (divide* tmp right)))
(defmethod divide* ((left amount) (right amount))
  ;; Increase the value's precision, to capture fractional parts after
  ;; the divide.  Round up in the last position.
  (if (value-zerop right)
      (error 'amount-error :msg
	     (format nil "Attempt to divide by zero: ~A / ~A"
		     (format-value left :full-precision-p t)
		     (format-value right :full-precision-p t))))

  (setf (amount-quantity left)
	(/ (* (amount-quantity left)
	      (expt 10 (+ (1+ *extra-precision*)
			  (* 2 (amount-precision right))
			  (amount-precision left))))
	   (amount-quantity right)))
  (setf (amount-precision left)
	(+ *extra-precision* (* 2 (amount-precision left))
	   (amount-precision right)))

  (setf (amount-quantity left)
	(cl:round (amount-quantity left)
		  (expt 10 (- (1+ (amount-precision left))
			      (amount-precision left)))))

  (set-amount-commodity-and-round* left right))

(defmethod divide ((left balance) (right amount)))
(defmethod divide* ((left balance) (right amount))
  ;; jww (2007-10-22): NYI
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
  )

(defmethod divide ((left amount) (right balance))
  (error 'amount-error :msg "Cannot divide an amount by a balance"))
(defmethod divide* ((left amount) (right balance))
  (error 'amount-error :msg "Cannot divide an amount by a balance"))

(defmethod divide ((left balance) (right balance))
  (error 'amount-error :msg "Cannot divide an amount by a balance"))
(defmethod divide* ((left balance) (right balance))
  (error 'amount-error :msg "Cannot divide an amount by a balance"))

;;;_ * BALANCE specific

;;;_  + Optimize the given value, returning its cheaper equivalent

(defmethod optimize-value ((integer integer))
  integer)

(defmethod optimize-value ((amount amount))
  (if (value-zerop* amount)
      0
      amount))

(defmethod optimize-value ((balance balance))
  (the (or balance amount integer)
    (block nil
      (let ((amounts-map (get-amounts-map balance)))
	(if (or (null amounts-map)
		(cl:zerop (hash-table-count amounts-map)))
	    0
	    (if (= 1 (hash-table-count amounts-map))
		(prog1
		    0
		  (maphash #'(lambda (commodity amount)
			       (declare (ignore commodity))
			       (return (optimize-value amount)))
			   amounts-map))
		balance))))))

;;;_  + Print and format AMOUNT and BALANCE

(defmethod print-value ((amount amount) &key
			(output-stream *standard-output*)
			(omit-commodity-p nil)
			(full-precision-p nil)
			(width nil)
			latter-width)
  (declare (type stream output-stream))
  (declare (type boolean omit-commodity-p))
  (declare (type boolean full-precision-p))
  (declare (type (or fixnum null) width))
  (declare (ignore latter-width))
  ;; jww (2007-10-17): This should change from a simple boolean to registered
  ;; commodity to which values should be converted (possibly in both
  ;; directions)
  ;;(unless (slot-value amount 'keep-base)
  ;;  ;; amount_t base(*this);
  ;;  ;; if (! amount_t::keep_base)
  ;;  ;;   base.in_place_unreduce();
  ;;  )

  (let* ((commodity (amount-commodity amount))
	 (omit-commodity-p (or omit-commodity-p (null commodity)))
	 (commodity-symbol (and (not omit-commodity-p) commodity
				(commodity-symbol commodity)))
	 (precision (amount-precision amount))
	 (display-precision
	  (if (or (null commodity)
		  full-precision-p
		  (amount-keep-precision-p amount))
	      (amount-precision amount)
	      (display-precision amount))))

    (assert (or (null commodity-symbol)
		(> (length (commodity-symbol-name commodity-symbol)) 0)))

    (multiple-value-bind (quotient remainder)
	(truncate (amount-quantity amount)
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
	  (princ (commodity-name amount) output-stream)
	  (maybe-gap))

	(format output-stream "~:[~v,' ,vD~;~v,' ,v:D~]" ;
		(and commodity
		     (commodity-thousand-marks-p commodity))
		width (if *european-style* #\. #\,) quotient)

	(unless (cl:zerop display-precision)
	  (format output-stream "~C~v,'0D"
		  (if *european-style* #\, #\.)
		  display-precision (cl:abs remainder)))
      
	(when (and (not omit-commodity-p)
		   (not (commodity-symbol-prefixed-p commodity-symbol)))
	  (maybe-gap)
	  (princ (commodity-name amount) output-stream)))

      (if (and (not omit-commodity-p)
	       commodity
	       (commodity-annotated-p commodity))
	  (format-commodity-annotation (commodity-annotation commodity)
				       :output-stream output-stream)))))

(defmethod format-value ((amount amount) &key
			 (omit-commodity-p nil) (full-precision-p nil)
			 (width nil) (latter-width nil))
  (with-output-to-string (out)
    (print-value amount :output-stream out
		 :omit-commodity-p omit-commodity-p
		 :full-precision-p full-precision-p
		 :width width
		 :latter-width latter-width)
    out))

(defmethod print-value ((balance balance) &key
			(output-stream *standard-output*)
			(omit-commodity-p nil)
			(full-precision-p nil)
			(width 12)
			(latter-width nil))
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
  (declare (type boolean omit-commodity-p))
  (declare (type boolean full-precision-p))
  (declare (type (or fixnum null) latter-width))
  (assert output-stream)
  (assert omit-commodity-p)
  (assert full-precision-p)
  (assert width)
  (assert latter-width)
  ;; jww (2007-10-22): NYI
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
  )

(defun quantity-string (amount)
  (assert amount)
  (format-value amount :omit-commodity-p t :full-precision-p t))

;;;_  + Find AMOUNT of COMMODITY in a BALANCE

(defmethod amount-in-balance ((balance balance) (commodity commodity))
  (or (gethash commodity (get-amounts-map balance))
      (unless (and *keep-annotation-prices*
		   *keep-annotation-dates*
		   *keep-annotation-tags*)
	(gethash commodity
		 (strip-annotations (get-amounts-map balance)
				    :keep-price *keep-annotation-prices*
				    :keep-date  *keep-annotation-dates*
				    :keep-tag   *keep-annotation-tags*)))))

;;;_ * COMMODITY

;;;_  + Return copy of AMOUNT without a commodity

(defun amount-sans-commodity (amount)
  (declare (type amount amount))
  (the amount
    (let ((commodity (amount-commodity amount)))
      (if commodity
	  (let ((tmp (copy-amount amount)))
	    (assert (amount-commodity tmp))
	    (setf (amount-commodity tmp) nil)
	    tmp)
	  amount))))

;;;_  - Parse commodity symbols

(declaim (inline symbol-char-invalid-p))

(defun symbol-char-invalid-p (c)
  (declare (type character c))
  (let ((code (char-code c)))
    (and (< code 256)
	 (aref +invalid-symbol-chars+ code))))

(declaim (inline symbol-name-needs-quoting-p))

(defun symbol-name-needs-quoting-p (name)
  "Return T if the given symbol NAME requires quoting."
  (declare (type string name))
  (the boolean
    (loop for c across name do
	 (if (symbol-char-invalid-p c)
	     (return t)))))

(define-condition commodity-error (error) 
  ((description :reader error-description :initarg :msg))
  (:report
   (lambda (condition stream)
     (format stream "~S" (error-description condition)))))

(defun read-commodity-symbol (in)
  "Parse a commodity symbol from the input stream IN.
  This is the correct entry point for creating a new commodity symbol.

  A commodity contain any character not found in `+invalid-symbol-chars+'.
  To include such characters in a symbol name -- except for #\\\", which may
  never appear in a symbol name -- surround the commodity name with double
  quotes.  It is an error if EOF is reached without reading the ending double
  quote.  If the symbol name is not quoted, and an invalid character is
  reading, reading from the stream stops and the invalid character is put
  back."
  (declare (type stream in))
  (let ((buf (make-string-output-stream))
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

;;;_  * Access commodity details (across all commodity classes)

;; The commodity and annotated-commodity classes are the main interface class
;; for dealing with commodities themselves (which most people will never do).

(defmethod commodity-base ((null null)) nil)
(defmethod commodity-base ((commodity commodity))
  (get-basic-commodity commodity))
(defmethod commodity-base ((annotated-commodity annotated-commodity))
  (get-basic-commodity (get-referent-commodity annotated-commodity)))

(defmacro commodity-indirection (name accessor &key type)
  `(progn
     (defmethod ,name ((commodity commodity))
       (the ,type
	 (,accessor (get-basic-commodity commodity))))
     (defmethod ,name ((annotated-commodity annotated-commodity))
       (the ,type
	 (,accessor (get-basic-commodity
		     (get-referent-commodity annotated-commodity)))))))

(commodity-indirection commodity-symbol get-symbol :type commodity-symbol)
(commodity-indirection commodity-description get-description :type string)
(commodity-indirection commodity-thousand-marks-p get-thousand-marks-p
		       :type boolean)
(commodity-indirection commodity-precision get-display-precision :type fixnum)

(defmethod commodity-name ((amount amount))
  (let ((commodity (amount-commodity amount)))
    (if (and commodity (commodity-annotated-p commodity))
	(setq commodity (get-referent-commodity commodity)))
    (if commodity
	(if (slot-boundp commodity 'qualified-name)
	    (commodity-qualified-name commodity)
	    (let ((symbol (get-symbol (get-basic-commodity commodity))))
	      (if symbol
		  (progn
		    (assert (not (commodity-symbol-needs-quoting-p symbol)))
		    (commodity-symbol-name symbol))))))))

(defmethod display-precision ((commodity commodity))
  (get-display-precision (get-basic-commodity commodity)))
(defmethod display-precision ((annotated-commodity annotated-commodity))
  (display-precision (get-referent-commodity annotated-commodity)))
(defmethod display-precision ((amount amount))
  (declare (type amount amount))
  (the fixnum
    (commodity-precision (amount-commodity amount))))

;;;_  + Commodity equality

(defmethod commodity-equal ((a null) (b null)) t)
(defmethod commodity-equal ((a commodity) (b null)) nil)
(defmethod commodity-equal ((a null) (b commodity)) nil)
(defmethod commodity-equal ((a commodity) (b commodity))
  "Two commodities are EQUAL if they have the same BASIC-COMMODITY.
  They may be of different COMMODITY types, but this is just nomenclature
  (i.e., USD always equals $)."
  (eq (get-basic-commodity a) (get-basic-commodity b)))
(defmethod commodity-equal ((a commodity) (b annotated-commodity)) nil)
(defmethod commodity-equal ((a annotated-commodity) (b commodity)) nil)
(defmethod commodity-equal ((a annotated-commodity) (b annotated-commodity))
  (eq a b))

(defmethod commodity-equalp ((a null) (b null)) t)
(defmethod commodity-equalp ((a commodity) (b null)) nil)
(defmethod commodity-equalp ((a null) (b commodity)) nil)
(defmethod commodity-equalp ((a commodity) (b commodity))
  "Two commodities are considered EQUALP if they refer to the same base."
  (eq (get-basic-commodity a) (get-basic-commodity b)))
(defmethod commodity-equalp ((a commodity) (b annotated-commodity))
  (commodity-equalp a (get-referent-commodity b)))
(defmethod commodity-equalp ((a annotated-commodity) (b commodity))
  (commodity-equalp (get-referent-commodity a) b))
(defmethod commodity-equalp ((a annotated-commodity) (b annotated-commodity))
  (commodity-equalp (get-referent-commodity a)
		    (get-referent-commodity b)))

;;;_  + Function to sort commodities

(defun commodity-lessp (left right)
  "Return T if commodity LEFT should be sorted before RIGHT."
  (declare (type (or commodity annotated-commodity null) left))
  (declare (type (or commodity annotated-commodity null) right))
  (the boolean
    (block nil
      (if (and (null left) right)
	  (return t))
      (if (and left (null right))
	  (return t))

      (unless (commodity-equal left right)
	(return (string-lessp (commodity-name left)
			      (commodity-name right))))

      (if (and (not (commodity-annotated-p left))
	       (commodity-annotated-p right))
	  (return t))
      (if (and (commodity-annotated-p left)
	       (not (commodity-annotated-p right)))
	  (return nil))

      (let ((left-annotation (commodity-annotation left))
	    (right-annotation (commodity-annotation right)))

	(let ((left-price (annotation-price left-annotation))
	      (right-price (annotation-price right-annotation)))
	  (if (and (not left-price) right-price)
	      (return t))
	  (if (and left-price (not right-price))
	      (return nil))

	  (if (and left-price right-price)
	      (setq left-price (smallest-units left-price)
		    right-price (smallest-units right-price))

	      (if (commodity-equal (amount-commodity left-price)
				   (amount-commodity right-price))
		  (return (value< left-price right-price))
		  ;; Since we have two different amounts, there's really no way
		  ;; to establish a true sorting order; we'll just do it based
		  ;; on the numerical values.
		  (return (< (amount-quantity left-price)
			     (amount-quantity right-price))))))

	(let ((left-date (annotation-date left-annotation))
	      (right-date (annotation-date right-annotation)))
	  (if (and (not left-date) right-date)
	      (return t))
	  (if (and left-date (not right-date))
	      (return nil))

	  (when (and left-date right-date)
	    (return (< left-date right-date))))

	(let ((left-tag (annotation-tag left-annotation))
	      (right-tag (annotation-tag right-annotation)))
	  (if (and (not left-tag) right-tag)
	      (return t))
	  (if (and left-tag (not right-tag))
	      (return nil))

	  (when (and left-tag right-tag)
	    (return (string-lessp left-tag right-tag)))))

      (return t))))

;;;_  + Current and historical market values (prices) for a commodity

(defun add-price (commodity price &optional datetime)
  (declare (type (or commodity annotated-commodity null) commodity))
  (declare (type amount price))
  (declare (type (or datetime null) datetime))
  (when commodity
    (let ((base (commodity-base commodity))
	  (pricing-entry
	   (make-pricing-entry :moment (or datetime
					   (get-universal-time)) :price price)))
      (if (not (get-price-history base))
	  (setf (get-price-history base) (rbt:nil-tree)))
      (let ((history (get-price-history base)))
	(multiple-value-bind (new-root node-inserted-or-found item-already-in-p)
	    (rbt:insert-item pricing-entry history :key 'pricing-entry-moment)
	  (if item-already-in-p
	      (setf (pricing-entry-price node-inserted-or-found) price))
	  (setf (get-price-history base) new-root)))
      price)))

(defun remove-price (commodity datetime)
  (declare (type (or commodity annotated-commodity null) commodity))
  (declare (type datetime datetime))
  (when commodity
    (let ((base (commodity-base commodity)))
      (when (get-price-history base)
	(multiple-value-bind (new-root node-deleted-p)
	    (rbt:delete-item datetime (get-price-history base)
			     :key 'pricing-entry-moment)
	  (setf (get-price-history base) new-root)
	  node-deleted-p)))))

(defun find-nearest (it root &key (test #'<=) (key #'identity))
  "Find an item in the tree which is closest to IT according to TEST.
  For the default, <=, this means no other item will be more less than
  IT in the tree than the one found."
  (declare (type datetime it))
  (declare (type rbt:rbt-node root))
  (the (or pricing-entry null)
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
	     (setq last-found p p (rbt:right p)))
	   ;; If the current item does not meet the test, there might be a
	   ;; candidate to the left -- but definitely not the right.
	   (setq p (rbt:left p))))))

(defun get-price-quote (symbol &optional datetime)
  (declare (type commodity-symbol symbol))
  (declare (type (or datetime null) datetime))
  (declare (ignore symbol))
  (declare (ignore datetime))
  (values))

(defmethod market-value ((commodity commodity) &optional datetime)
  (declare (type (or commodity annotated-commodity null) commodity))
  (declare (type (or datetime null) datetime))

  (the (or amount null)
    (let* ((base (commodity-base commodity))
	   (history (get-price-history base))
	   pricing-entry)
      (when history
	(if (null datetime)
	    (progn
	      (loop while (not (rbt:rbt-null (rbt:right history)))
		 do (setq history (rbt:right history)))
	      (assert history)
	      (setq pricing-entry (rbt:node-item history)))
	    (setq pricing-entry
		  (find-nearest datetime history
				:key 'pricing-entry-moment)))
	(assert pricing-entry)		; pricing entry
	)

      ;; jww (2007-10-24): When is the right time to download a price quote?
      ;;      (unless (or (commodity-no-market-price-p commodity)
      ;;		  (null *get-price-quote-function*)
      ;;		  (and (get-last-lookup commodity)
      ;;		       (< (-))))
      ;;	(get-price-quote commodity datetime pricing-entry))
      )))

(defmethod market-value ((annotated-commodity annotated-commodity) &optional datetime)
  (market-value (get-referent-commodity annotated-commodity) datetime))

(defmethod market-value ((amount amount) &optional datetime)
  (let ((commodity (amount-commodity amount)))
    (when commodity
      (let ((value (market-value commodity datetime)))
	(if value
	    (value-round* (multiply* value amount)))))))

(defmethod market-value ((balance balance) &optional datetime)
  (let ((value-balance (make-instance 'balance)))
    (maphash #'(lambda (commodity amount)
		 (declare (ignore commodity))
		 (add* value-balance
		       (market-value amount datetime)))
	     (get-amounts-map balance))
    value-balance))

;;;_ * AMOUNT and COMMODITY

;;;_  + Unit conversions

;; jww (2007-10-24): With all of these functions, I cannot just call
;; `get-basic-commodity' directly, if it's an annotated commodity.

(defmethod smaller-units ((amount amount))
  (unless (null (amount-commodity amount))
    (let* ((commodity (amount-commodity amount))
	   (equiv-amount (get-smaller-unit-equivalence
			  (commodity-base commodity))))
      (if equiv-amount
	  (multiply equiv-amount amount)
	  amount))))

(defmethod smaller-units ((balance balance))
  (assert balance))

(defmethod smallest-units ((amount amount))
  (unless (null (amount-commodity amount))
    (do* ((commodity (amount-commodity amount)
		     (amount-commodity amount))
	  (equiv-amount (get-smaller-unit-equivalence
			 (commodity-base commodity))
			(get-smaller-unit-equivalence
			 (commodity-base commodity))))
	 ((null equiv-amount) amount)
      (setq amount (multiply equiv-amount amount)))
    amount))

(defmethod smallest-units ((balance balance))
  (assert balance))

(defmethod larger-units ((amount amount))
  (unless (null (amount-commodity amount))
    (do* ((commodity (amount-commodity amount)
		     (amount-commodity amount))
	  (equiv-amount (get-larger-unit-equivalence
			 (commodity-base commodity))
			(get-larger-unit-equivalence
			 (commodity-base commodity))))
	 ((null equiv-amount) amount)
      (let ((new-amount (multiply equiv-amount amount)))
	(if (< (amount-to-integer new-amount) 1)
	    (return amount)
	    (setq amount new-amount))))))

(defmethod larger-units ((balance balance))
  (assert balance))

(defmethod convert-to-units ((amount amount) (commodity commodity))
  ;; jww (2007-10-24): The `do' loop here are broken
  (unless (or (null (amount-commodity amount))
	      (null commodity))
    (block nil
      ;; See if the commodity is smaller than the current one
      (let ((new-amount amount))
	(do ((equiv-amount
	      (get-smaller-unit-equivalence
	       (commodity-base (amount-commodity amount)))))
	    ((null equiv-amount))
	  (setq new-amount (multiply equiv-amount new-amount))
	  (if (commodity-equal (amount-commodity new-amount)
			       commodity)
	      (return new-amount))))

      ;; No, see if it's larger than the current one
      (let ((new-amount amount))
	(do ((equiv-amount
	      (get-larger-unit-equivalence
	       (commodity-base (amount-commodity new-amount)))))
	    ((null equiv-amount))
	  (setq new-amount (multiply equiv-amount new-amount))
	  (if (commodity-equal (amount-commodity new-amount)
			       commodity)
	      (return new-amount))))

      ;; No, we couldn't convert to the given units
      amount)))

(defmethod convert-to-units ((balance balance) (commodity commodity))
  (assert balance))

(defun set-commodity-equivalence (base-amount equivalent-amount)
  (unless (or (null (amount-commodity base-amount))
	      (null (amount-commodity equivalent-amount)))
    (multiple-value-bind (quotient remainder)
	(truncate (amount-quantity base-amount)
		  (expt 10 (amount-precision base-amount)))

      (unless (and (cl:zerop remainder)
		   (= 1 quotient))
	(error 'amount-error :msg
	       "First arg to `set-commodity-equivalence' must be of a single unit"))
      (unless (> 1 (nth-value 0 (truncate
				 (amount-quantity equivalent-amount)
				 (expt 10 (amount-precision equivalent-amount)))))
	(error 'amount-error :msg
	       "Second arg to `set-commodity-equivalence' must be more than a single unit"))

      (let ((commodity (amount-commodity base-amount))
	    (equiv-commodity (amount-commodity equivalent-amount)))
	(setf (get-smaller-unit-equivalence
	       (commodity-base commodity)) equivalent-amount)
	(setf (get-larger-unit-equivalence
	       (commodity-base equiv-commodity))
	      (divide base-amount equivalent-amount))))))

;;;_  + Exchange a commodity

(defun exchange-commodity (amount price &key
			   (sale nil) (moment nil) (note nil))
  (declare (type amount amount))
  (declare (type amount price))
  (declare (type boolean sale))
  (declare (type (or datetime null) moment))
  (declare (type (or string null) note))
  (the (values amount (or amount null))
    (let ((current-annotation (commodity-annotation
			       (amount-commodity amount)))
	  (per-share-price (divide price amount)))
      (values
       (if sale
	   (and current-annotation
		(subtract price
			  (multiply (annotation-price current-annotation)
				    amount)))
	   (annotate-commodity
	    amount
	    (make-commodity-annotation :price per-share-price
				       :date moment
				       :tag note)))))))

(defun purchase-commodity (amount price &key (moment nil) (note nil))
  (exchange-commodity amount price :moment moment :note note))

(defun sell-commodity (amount price &key (moment nil) (note nil))
  (exchange-commodity amount price :sale t :moment moment :note note))

;;;_ * COMMODITY-POOL

;;;_  - Commodity creation and pool management

;; All commodities are allocated within a pool, which can be used to look them
;; up.

(defun reset-commodity-pool (&optional pool)
  (setq *default-commodity-pool* (or pool (make-commodity-pool))))

;;;_   : COMMODITY

(defun create-commodity (name &key (pool *default-commodity-pool*))
  "Create a COMMODITY after the symbol name found by parsing NAME.
  The NAME can be either a string or an input stream, or nil, in which
  case the name is read from *standard-input*.
  The argument :pool specifies the commodity pool which will maintain
  this commodity, and by which other code may access it again.
  The resulting COMMODITY object is returned."
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (the commodity
    (let* ((symbol (if (stringp name)
		       (with-input-from-string (in name)
			 (read-commodity-symbol in))
		       name))
	   (base (make-basic-commodity :symbol symbol))
	   (commodity (make-instance 'commodity :basic-commodity base
						:commodity-pool pool))
	   (symbol-name (commodity-symbol-name symbol)))

      (if (commodity-symbol-needs-quoting-p symbol)
	  (setf (commodity-qualified-name commodity)
		(concatenate 'string "\"" symbol-name "\"")))

      (let ((commodities-by-serial-list
	     (commodity-pool-by-serial-list pool)))
	(setf (commodity-serial-number commodity)
	      (1+ (caar (last commodities-by-serial-list))))
	(nconc commodities-by-serial-list
	       (list (cons (commodity-serial-number commodity)
			   commodity))))

      (let ((names-map (commodity-pool-by-name-map pool)))
	(assert (not (gethash symbol-name names-map)))
	(setf (gethash symbol-name names-map) commodity)))))

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
  (declare (type boolean create-if-not-exists-p))
  (declare (type commodity-pool pool))
  (the (values (or commodity null) boolean)
    (let ((by-name-map (commodity-pool-by-name-map pool)))
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
		(values nil nil)))))))

(defun find-commodity-by-serial (serial &key (pool *default-commodity-pool*))
  "Find the commodity with the matching unique SERIAL number.
  nil is returned if no such commodity exists."
  (declare (type fixnum serial))
  (declare (type commodity-pool pool))
  (the (or commodity annotated-commodity)
    (cdr (assoc serial (commodity-pool-by-serial-list pool)))))

;;;_   : ANNOTATED-COMMODITY

(defun make-qualified-name (commodity commodity-annotation)
  (declare (type commodity commodity))
  (declare (type commodity-annotation commodity-annotation))

  (if (and (annotation-price commodity-annotation)
	   (< (sign (annotation-price commodity-annotation)) 0))
      (error 'amount-error :msg "A commodity's price may not be negative"))

  (with-output-to-string (out)
    (princ (commodity-symbol-name
	    (commodity-symbol commodity)) out)
    (format-commodity-annotation commodity-annotation :output-stream out)))

(defun create-annotated-commodity (commodity details qualified-name)
  "Create an ANNOTATED-COMMODITY which annotates COMMODITY.
  The NAME can be either a string or a COMMODITY-SYMBOL."
  (declare (type commodity commodity))
  (declare (type commodity-annotation details))
  (declare (type string qualified-name))
  (the annotated-commodity
    (let ((annotated-commodity
	   (make-instance 'annotated-commodity
			  :referent-commodity commodity
			  :annotation details
			  :qualified-name qualified-name))
	  (pool (get-commodity-pool commodity)))

      (let ((commodities-by-serial-list
	     (commodity-pool-by-serial-list pool)))
	(setf (commodity-serial-number commodity)
	      (1+ (caar (last commodities-by-serial-list))))
	(nconc commodities-by-serial-list
	       (list (cons (commodity-serial-number commodity)
			   annotated-commodity))))

      (let ((names-map (commodity-pool-by-name-map pool)))
	(assert (not (gethash qualified-name names-map)))
	(setf (gethash qualified-name names-map) annotated-commodity)))))

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
  (declare (type boolean create-if-not-exists-p))
  (declare (type commodity-pool pool))
  (assert (not (commodity-annotation-empty-p details)))
  (the (values (or annotated-commodity null) boolean)
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
	  (values nil nil)))))

;;;_ * ANNOTATED-COMMODITY

;;;_  - Read commodity annotation from a stream

(defun read-until (in char &optional error-message)
  (declare (type stream in))
  (declare (type character char))
  (declare (type (or string null) error-message))
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
	     (if (annotation-price annotation)
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
			  (< (amount-precision tmp-amount)
			     (display-precision commodity)))
		     (setq tmp-amount
			   (value-round* tmp-amount
					 (display-precision commodity)))))

	       (setf (annotation-price annotation) tmp-amount)))

	    ((char= #\[ c)
	     (if (annotation-date annotation)
		 (error 'amount-error :msg
			"Commodity annotation specifies more than one date"))

	     ;; jww (2007-10-20): This code cannot work until I have a decent
	     ;; Date/Time library for Common Lisp.

	     (read-char in)
	     ;;(let* ((date-string
	     ;;	    (read-until in #\] "Commodity date lacks closing bracket"))
	     ;;	   (tmp-date (parse-datetime date-string)))
	     ;;  (setf (annotation-date annotation) tmp-date))
	     )

	    ((char= #\( c)
	     (if (annotation-tag annotation)
		 (error 'amount-error :msg
			"Commodity annotation specifies more than one tag"))

	     (read-char in)
	     (setf (annotation-tag annotation)
		   (read-until in #\) "Commodity tag lacks closing parenthesis")))

	    (t
	     (return))))
    (the commodity-annotation
      annotation)))

;;;_  - Format commodity annotation to a string

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
  (declare (type stream output-stream))
  (format output-stream "~:[~; {~:*~A}~]~:[~; [~:*~A]~]~:[~; (~:*~A)~]"
	  (format-value (annotation-price annotation))
	  (annotation-date annotation)
	  (annotation-tag annotation)))

;;;_  + Unary truth tests

(defmethod commodity-annotated-p ((commodity commodity))
  (assert (not (slot-boundp commodity 'annotated-p)))
  nil)
(defmethod commodity-annotated-p ((annotated-commodity annotated-commodity))
  (assert (slot-boundp annotated-commodity 'annotated-p))
  (assert (get-annotated-p annotated-commodity))
  t)
(defmethod commodity-annotated-p ((amount amount))
  (commodity-annotated-p (amount-commodity amount)))

;;;_  + Annotate the commodity of a COMMODITY or AMOUNT

(defmethod annotate-commodity ((commodity commodity)
			       (details commodity-annotation))
  (find-annotated-commodity commodity details :create-if-not-exists-p t))

(defmethod annotate-commodity ((amount amount)
			       (details commodity-annotation))
  (let ((commodity (amount-commodity amount)))
    (unless commodity
      (error 'amount-error :msg
	     "Cannot annotate an amount which has no commodity"))
    (let ((referent commodity))
      (if (commodity-annotated-p referent)
	  (setq referent (get-referent-commodity referent)))

      (let ((annotated-commodity
	     (find-annotated-commodity referent details
				       :create-if-not-exists-p t)))
	(let ((tmp (copy-amount amount)))
	  (setf (amount-commodity tmp) annotated-commodity)
	  tmp)))))

;;;_  + Access a commodity's annotation

(defmethod commodity-annotation ((commodity commodity))
  nil)
(defmethod commodity-annotation ((annotated-commodity annotated-commodity))
  (get-annotation annotated-commodity))
(defmethod commodity-annotation ((amount amount))
  ;; This calls the appropriate generic function
  (commodity-annotation (amount-commodity amount)))

;;;_  + Commodity annotation tests

(defmethod commodity-annotation-empty-p ((annotation commodity-annotation))
  (not (or (annotation-price annotation)
	   (annotation-date annotation)
	   (annotation-tag annotation))))

(defmethod commodity-annotation-equal ((a commodity-annotation)
				       (b commodity-annotation))
  (let ((price-a (annotation-price a))
	(price-b (annotation-price b))
	(date-a (annotation-date a))
	(date-b (annotation-date b))
	(tag-a (annotation-tag a))
	(tag-b (annotation-tag b)))
    (and (or (and (null price-a) (null price-b))
	     (and price-a price-b
		  (value= price-a price-b)))
	 (or (and (null date-a) (null date-b))
	     (and date-a date-b
		  (= date-a date-b)))
	 (or (and (null tag-a) (null tag-b))
	     (and tag-a tag-b
		  (string= tag-a tag-b))))))

;;;_  + Strip commodity annotations

;; annotated-commodity's are references to other commodities (which in turn
;; reference a basic-commodity) which carry along additional contextual
;; information relating to a point in time.

(defmethod strip-annotations ((commodity commodity)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (declare (ignore keep-price))
  (declare (ignore keep-date))
  (declare (ignore keep-tag))
  (assert (not (commodity-annotated-p commodity)))
  commodity)

(defmethod strip-annotations ((annotated-commodity annotated-commodity)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (assert (commodity-annotated-p annotated-commodity))
  (the (or annotated-commodity commodity)
    (let* ((annotation (commodity-annotation annotated-commodity))
	   (price (annotation-price annotation))
	   (date (annotation-date annotation))
	   (tag (annotation-tag annotation)))
      (if (and (or keep-price (null price))
	       (or keep-date (null date))
	       (or keep-tag (null tag)))
	  annotated-commodity
	  (let ((tmp (make-instance 'annotated-commodity)))
	    (setf (get-referent-commodity tmp)
		  (get-referent-commodity annotated-commodity))
	    (let ((new-ann (make-commodity-annotation)))
	      (setf (get-annotation tmp) new-ann)
	      (if keep-price
		  (setf (annotation-price new-ann) price))
	      (if keep-date
		  (setf (annotation-date new-ann) date))
	      (if keep-tag
		  (setf (annotation-tag new-ann) tag)))
	    (if (commodity-annotation-empty-p tmp)
		(get-referent-commodity tmp)
		tmp))))))

(defmethod strip-annotations ((amount amount)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (the amount
    (if (and keep-price keep-date keep-tag)
	amount
	(let ((commodity (amount-commodity amount)))
	  (if (or (null commodity)
		  (not (commodity-annotated-p commodity))
		  (and keep-price keep-date keep-tag))
	      amount
	      (let ((tmp (copy-amount amount)))
		(setf (amount-commodity tmp)
		      (strip-annotations commodity
					 :keep-price keep-price
					 :keep-date keep-date
					 :keep-tag keep-tag))
		tmp))))))

(defmethod strip-annotations ((balance balance)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (the balance
    (if (and keep-price keep-date keep-tag)
	balance
	(let ((stripped-balance (make-instance 'balance)))
	  (maphash #'(lambda (commodity amount)
		       (declare (ignore commodity))
		       (add* stripped-balance
			     (strip-annotations amount
						:keep-price keep-price
						:keep-date  keep-date
						:keep-tag   keep-tag)))
		   (get-amounts-map balance))
	  stripped-balance))))

;; cambl.lisp ends here
