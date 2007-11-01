;; cambl-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cambl)
  (require :xlunit))

(defpackage :cambl-test
  (:use :cl :cambl :xlunit)
  (:export :cambl-test-suite
	   :commodity-test-case
	   :amount-test-case))

(in-package :cambl-test)

(defclass commodity-test-case (test-case)
  ()
  (:documentation "test-case for CAMBL commodities"))

(defun assert-valid (object)
  (assert object))

(defun assert-value-equal (left right)
  (assert (value= left right)))

(defun assert-value-not-equal (left right)
  (assert (not (value= left right))))

(def-test-method test-price-history ((test commodity-test-case) :run nil)
  ;; ptime jan17_07    = parse_datetime("2007/01/17 00:00:00");
  ;; ptime feb27_07    = parse_datetime("2007/02/27 18:00:00");
  ;; ptime feb28_07    = parse_datetime("2007/02/28 06:00:00");
  ;; ptime feb28_07sbm = parse_datetime("2007/02/28 11:59:59");
  ;; ptime mar01_07    = parse_datetime("2007/03/01 00:00:00");
  ;; ptime apr15_07    = parse_datetime("2007/04/15 13:00:00");

  ;; // jww (2007-04-17): tbd
  ;; amount_t x0;
  ;; amount_t x1("100.10 AAPL");

  ;; assert-condition(x0.value(), amount_error);
  ;; assert-false(x1.value());

  ;; // Commodities cannot be constructed by themselves, since a great
  ;; // deal of their state depends on how they were seen to be used.
  ;; commodity_t& aapl(x1.commodity());

  ;; aapl.add_price(jan17_07, amount_t("$10.20"));
  ;; aapl.add_price(feb27_07, amount_t("$13.40"));
  ;; aapl.add_price(feb28_07, amount_t("$18.33"));
  ;; aapl.add_price(feb28_07sbm, amount_t("$18.30"));
  ;; aapl.add_price(mar01_07, amount_t("$19.50"));
  ;; aapl.add_price(apr15_07, amount_t("$21.22"));

  ;; optional<amount_t> amt1 = x1.value(feb28_07sbm);
  ;; assert-true(amt1);
  ;; assert-value-equal(amount_t("$1831.83"), *amt1);

  ;; optional<amount_t> amt2 = x1.value(now);
  ;; assert-true(amt2);
  ;; assert-value-equal(amount_t("$2124.12"), *amt2);

  ;; assert-valid(x1);
  )

(def-test-method test-lots ((test commodity-test-case) :run nil)
  ;; // jww (2007-04-17): tbd
  )

(def-test-method test-scaling-base ((test commodity-test-case) :run nil)
  ;; // jww (2007-04-17): tbd
  )

(def-test-method test-reduction ((test commodity-test-case) :run nil)
  ;; // jww (2007-04-17): tbd
  )

(defclass amount-test-case (test-case)
  ()
  (:documentation "test-case for CAMBL amounts"))

(defvar original-*european-style* nil)

(defmethod set-up ((test amount-test-case))
  ;; Cause the display precision for dollars to be initialized to 2.
  (setq original-*european-style* *european-style*
	*european-style* nil)
  (reset-commodity-pool)
  (amount "$1,000.00"))

(defmethod tear-down ((test amount-test-case))
  (setq *european-style* original-*european-style*))

(defmacro define-test (name empty &rest body-forms)
  (declare (ignore empty))
  `(def-test-method ,name ((test amount-test-case) :run nil)
     ,@body-forms))

(define-test amount/uncommoditized ()
  (assert-equal "0" (format-value (amount "0")))
  (assert-equal "0.10" (format-value (amount "0.10")))
  (assert-equal "0.10" (format-value (amount ".10")))
  (assert-equal "12.1000000000000" (format-value (amount "12.1000000000000")))
  (assert-equal "12.10000" (format-value (amount* "12.10000"))))

(define-test amount/commoditized ()
  (assert-equal "$0.00" (format-value (amount "$0")))
  (assert-equal "$0.10" (format-value (amount "$0.10")))
  (assert-equal "$12.10" (format-value (amount* "$12.1000000000000")))
  (assert-equal "$12.10" (format-value (amount* "$12.10000")))
  (assert-equal "$ 12.10" (format-value (amount "$ 12.10")))
  (assert-equal "$12.10" (format-value (amount "$12.10")))

  (assert-equal "DX 12.10001" (format-value (amount "DX 12.10001")))
  (assert-equal "DX 12.10000" (format-value (amount* "DX 12.1")))

  (assert-equal "12x" (format-value (amount "12x")))
  ;; jww (2007-10-25): This should be an error instead
  (assert-equal "12x" (format-value (amount "12x."))) ; ignore bogus chars

  (assert-condition 'end-of-file (amount "EUR"))
  ;; jww (2007-10-25): This should give a much better error
  ;;(assert-condition 'no-integer-present (amount "."))

  (let ((x12 (amount* "$100")))
    (assert-equal 2 (display-precision x12))
    (assert-equal 2 (display-precision (amount-commodity x12)))

    (with-input-from-string (in "$100...")
      (let ((x13 (read-amount* in)))
	(assert-value-equal x12 x13)
	(assert-equal 2 (display-precision x13))))

    (let ((*european-style* t))
      (assert-equal "$2.000,00"
		    (format-value (amount* "$2000")))
      (assert-equal "0" (format-value (amount "0")))
      (assert-equal "$0,00" (format-value (amount "$0")))
      (assert-equal "$2.000,00"
		    (format-value (amount* "$2,000.00")))
      (assert-equal "$2.000,00"
		    (format-value (amount* "$2.000,00"))))

    (let ((*european-style* nil))
      (assert-equal "$2,000.00"
		    (format-value (amount* "$2000")))
      (assert-equal "0" (format-value (amount "0")))
      (assert-equal "$0.00" (format-value (amount "$0")))
      (assert-equal "$2,000.00"
		    (format-value (amount* "$2,000.00")))
      (assert-equal "$2,000.00"
		    (format-value (amount* "$2.000,00"))))

    (let ((x15 (amount* "$2000"))
	  (x16 (amount* "$2,000")))
      (assert-equal "$2,000.00" (format-value x15))
      (assert-equal "$2,000.00" (format-value x16))
      (assert-value-equal x15 x16))

    (assert-equal "EUR 100" (format-value (amount "EUR 100")))

    (let ((x1 (amount* "$100.0000")))
      (assert-eql 2 (display-precision x12))
      (assert-eql (amount-commodity x1) (amount-commodity x12))
      (assert-value-equal x1 x12))

    (let ((x0 (amount "$100.0000")))
      (assert-eql 4 (display-precision x12)) ; should have changed now
      (assert-eql (amount-commodity x0) (amount-commodity x12))
      (assert-value-equal x0 x12))

    (let ((x2 (amount "$100.00" :reduce-to-smallest-units-p nil)))
      (assert-value-equal x2 x12))
    (let ((x3 (amount* "$100.00" :reduce-to-smallest-units-p nil)))
      (assert-value-equal x3 x12))

    (let ((x4 (amount "$100.00")))
      (assert-value-equal x4 x12))
    (let ((x5 (amount* "$100.00")))
      (assert-value-equal x5 x12))
    (let ((x6 (amount "$100.00" :reduce-to-smallest-units-p nil)))
      (assert-value-equal x6 x12))
    (let ((x7 (amount* "$100.00" :reduce-to-smallest-units-p nil)))
      (assert-value-equal x7 x12))

    (assert-valid x12)))

(defun read-string (function string)
  (with-input-from-string (in string)
    (funcall function in)))

(define-test read-amount/commoditized ()
  (assert-equal "$0.00" (format-value (read-string #'read-amount "$0")))
  (assert-equal "$0.10" (format-value (read-string #'read-amount "$0.10")))
  (assert-equal "$12.10" (format-value (read-string #'read-amount*
						    "$12.1000000000000")))
  (assert-equal "$12.10" (format-value (read-string #'read-amount*
						    "$12.10000")))

  (assert-equal "EUR 12.10001"
		(format-value (read-string #'read-amount "EUR 12.10001")))
  (assert-equal "EUR 12.10000"
		(format-value (read-string #'read-amount* "EUR 12.1"))))

(define-test exact-amount ()
  (assert-equal "$0.0000" (format-value (exact-amount "$0.0000")))
  (assert-equal "$0.00" (format-value (amount* "$0.0000"))))

(define-test read-exact-amount ()
  (assert-equal "$0.0000"
		(format-value (read-string #'read-exact-amount
					   "$0.0000"))))

(define-test float-to-amount ()
  (assert-equal "0.0" (format-value (float-to-amount 0.0)))
  (assert-equal "2.10005" (format-value (float-to-amount 2.10005)))
  (assert-equal "-2.10005" (format-value (float-to-amount -2.10005))))

(define-test integer-to-amount ()
  (assert-equal "0" (format-value (integer-to-amount 0)))
  (assert-equal "12072349872398572398723598723598723987235"
		(format-value (integer-to-amount
  12072349872398572398723598723598723987235))))

(define-test copy-amount ()
  (let* ((x (amount* "$123.45678"))
	 (copy (copy-amount x)))
    (assert-condition 'assert-error (assert-equal x copy))
    (assert-value-equal x copy)))

(define-test copy-amount ()
  (let* ((x (amount* "$123.45678"))
	 (copy (copy-value x)))
    (assert-condition 'assert-error (assert-equal x copy))
    (assert-value-equal x copy)))

(define-test value-zerop ()
  (assert-true (value-zerop (amount* "$0")))
  (assert-true (value-zerop (amount* "$0.00000000000")))
  (assert-true (value-zerop (amount* "EDU 0")))
  (assert-true (value-zerop (amount* "0")))
  (assert-true (value-zerop (amount* "0000000")))
  (assert-true (value-zerop (amount* "0.0000000000000")))
  ;; It's would display as zero...
  (assert-true (value-zerop (amount* "0.0000000000000000000000000000001")))
  ;; But it's not *really* zero
  (assert-false (value-zerop* (amount* "0.0000000000000000000000000000001"))))

(define-test value-plusp ()
  (assert-false (value-plusp (amount* "$0")))
  (assert-false (value-plusp (amount* "$0.00000000000")))
  (assert-false (value-plusp (amount* "EDU 0")))
  (assert-false (value-plusp (amount* "0")))
  (assert-false (value-plusp (amount* "0000000")))
  (assert-false (value-plusp (amount* "0.0000000000000")))
  ;; It's would display as zero...
  (assert-false (value-plusp (amount* "0.0000000000000000000000000000001")))
  ;; But it's *really* plusp
  (assert-true (value-plusp* (amount* "0.0000000000000000000000000000001")))

  (assert-true (value-plusp (amount* "$1")))
  (assert-false (value-plusp (amount* "$0.00000000001")))
  (assert-true (value-plusp (amount* "EDU 1")))
  (assert-true (value-plusp (amount* "1")))
  (assert-true (value-plusp (amount* "10000000")))
  ;; It's would display as zero...
  (assert-false (value-plusp (amount* "0.0000000000000000000000000000001")))
  ;; But it's not *really* zero
  (assert-true (value-plusp* (amount* "0.0000000000000000000000000000001"))))

(define-test value-minusp ()
  (assert-false (value-minusp (amount* "-$0")))
  (assert-false (value-minusp (amount* "-$0.00000000000")))
  (assert-false (value-minusp (amount* "EDU -0")))
  (assert-false (value-minusp (amount* "-0")))
  (assert-false (value-minusp (amount* "-0000000")))
  (assert-false (value-minusp (amount* "-0.0000000000000")))
  ;; It's would display as zero...
  (assert-false (value-minusp (amount* "-0.0000000000000000000000000000001")))
  ;; But it's *really* minusp
  (assert-true (value-minusp* (amount* "-0.0000000000000000000000000000001")))

  (assert-true (value-minusp (amount* "-$1")))
  (assert-false (value-minusp (amount* "-$0.00000000001")))
  (assert-true (value-minusp (amount* "-EDU 1")))
  (assert-true (value-minusp (amount* "-1")))
  (assert-true (value-minusp (amount* "-10000000")))
  ;; It's would display as zero...
  (assert-false (value-minusp (amount* "-0.0000000000000000000000000000001")))
  ;; But it's not *really* zero
  (assert-true (value-minusp* (amount* "-0.0000000000000000000000000000001"))))

(define-test constructors ()
  ;; amount_t x0;
  ;; amount_t x1(123456L);
  ;; amount_t x2(123456UL);
  ;; amount_t x3(123.456);
  ;; amount_t x5("123456");
  ;; amount_t x6("123.456");
  ;; amount_t x7(string("123456"));
  ;; amount_t x8(string("123.456"));
  ;; amount_t x9(x3);
  ;; amount_t x10(x6);
  ;; amount_t x11(x8);

  ;; assert-condition(amount_t(0L) == x0, amount_error);
  ;; assert-condition(amount_t() == x0, amount_error);
  ;; assert-condition(amount_t("0") == x0, amount_error);
  ;; assert-condition(amount_t("0.0") == x0, amount_error);
  ;; assert-value-equal(x2, x1);
  ;; assert-value-equal(x5, x1);
  ;; assert-value-equal(x7, x1);
  ;; assert-value-equal(x6, x3);
  ;; assert-value-equal(x8, x3);
  ;; assert-value-equal(x10, x3);
  ;; assert-value-equal(x10, x9);

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  ;; assert-valid(x11);
  )

(define-test commodity-constructors ()
  ;; amount_t x1("$123.45");
  ;; amount_t x2("-$123.45");
  ;; amount_t x3("$-123.45");
  ;; amount_t x4("DM 123.45");
  ;; amount_t x5("-DM 123.45");
  ;; amount_t x6("DM -123.45");
  ;; amount_t x7("123.45 euro");
  ;; amount_t x8("-123.45 euro");
  ;; amount_t x9("123.45€");
  ;; amount_t x10("-123.45€");

  ;; assert-value-equal(amount_t("$123.45"), x1);
  ;; assert-value-equal(amount_t("-$123.45"), x2);
  ;; assert-value-equal(amount_t("$-123.45"), x3);
  ;; assert-value-equal(amount_t("DM 123.45"), x4);
  ;; assert-value-equal(amount_t("-DM 123.45"), x5);
  ;; assert-value-equal(amount_t("DM -123.45"), x6);
  ;; assert-value-equal(amount_t("123.45 euro"), x7);
  ;; assert-value-equal(amount_t("-123.45 euro"), x8);
  ;; assert-value-equal(amount_t("123.45€"), x9);
  ;; assert-value-equal(amount_t("-123.45€"), x10);

  ;; assert-value-equal(string("$123.45"), x1.to_string());
  ;; assert-value-equal(string("$-123.45"), x2.to_string());
  ;; assert-value-equal(string("$-123.45"), x3.to_string());
  ;; assert-value-equal(string("DM 123.45"), x4.to_string());
  ;; assert-value-equal(string("DM -123.45"), x5.to_string());
  ;; assert-value-equal(string("DM -123.45"), x6.to_string());
  ;; assert-value-equal(string("123.45 euro"), x7.to_string());
  ;; assert-value-equal(string("-123.45 euro"), x8.to_string());
  ;; assert-value-equal(string("123.45€"), x9.to_string());
  ;; assert-value-equal(string("-123.45€"), x10.to_string());

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  )

(define-test assignment ()
  ;; amount_t x0;
  ;; amount_t x1;
  ;; amount_t x2;
  ;; amount_t x3;
  ;; amount_t x5;
  ;; amount_t x6;
  ;; amount_t x7;
  ;; amount_t x8;
  ;; amount_t x9;
  ;; amount_t x10;

  ;; x1  = 123456L;
  ;; x2  = 123456UL;
  ;; x3  = 123.456;
  ;; x5  = "123456";
  ;; x6  = "123.456";
  ;; x7  = string("123456");
  ;; x8  = string("123.456");
  ;; x9  = x3;
  ;; x10 = amount_t(x6);

  ;; assert-value-equal(x2, x1);
  ;; assert-value-equal(x5, x1);
  ;; assert-value-equal(x7, x1);
  ;; assert-value-equal(x6, x3);
  ;; assert-value-equal(x8, x3);
  ;; assert-value-equal(x10, x3);
  ;; assert-value-equal(x10, x9);

  ;; assert-false(x1.is_null());
  ;; x1 = x0;			// sets x1 back to uninitialized state
  ;; assert-true(x0.is_null());
  ;; assert-true(x1.is_null());

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  )

(define-test commodity-assignment ()
  ;; amount_t x1;
  ;; amount_t x2;
  ;; amount_t x3;
  ;; amount_t x4;
  ;; amount_t x5;
  ;; amount_t x6;
  ;; amount_t x7;
  ;; amount_t x8;
  ;; amount_t x9;
  ;; amount_t x10;

  ;; x1  = "$123.45";
  ;; x2  = "-$123.45";
  ;; x3  = "$-123.45";
  ;; x4  = "DM 123.45";
  ;; x5  = "-DM 123.45";
  ;; x6  = "DM -123.45";
  ;; x7  = "123.45 euro";
  ;; x8  = "-123.45 euro";
  ;; x9  = "123.45€";
  ;; x10 = "-123.45€";

  ;; assert-value-equal(amount_t("$123.45"), x1);
  ;; assert-value-equal(amount_t("-$123.45"), x2);
  ;; assert-value-equal(amount_t("$-123.45"), x3);
  ;; assert-value-equal(amount_t("DM 123.45"), x4);
  ;; assert-value-equal(amount_t("-DM 123.45"), x5);
  ;; assert-value-equal(amount_t("DM -123.45"), x6);
  ;; assert-value-equal(amount_t("123.45 euro"), x7);
  ;; assert-value-equal(amount_t("-123.45 euro"), x8);
  ;; assert-value-equal(amount_t("123.45€"), x9);
  ;; assert-value-equal(amount_t("-123.45€"), x10);

  ;; assert-value-equal(string("$123.45"), x1.to_string());
  ;; assert-value-equal(string("$-123.45"), x2.to_string());
  ;; assert-value-equal(string("$-123.45"), x3.to_string());
  ;; assert-value-equal(string("DM 123.45"), x4.to_string());
  ;; assert-value-equal(string("DM -123.45"), x5.to_string());
  ;; assert-value-equal(string("DM -123.45"), x6.to_string());
  ;; assert-value-equal(string("123.45 euro"), x7.to_string());
  ;; assert-value-equal(string("-123.45 euro"), x8.to_string());
  ;; assert-value-equal(string("123.45€"), x9.to_string());
  ;; assert-value-equal(string("-123.45€"), x10.to_string());

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  )

(define-test equality ()
  ;; amount_t x1(123456L);
  ;; amount_t x2(456789L);
  ;; amount_t x3(333333L);
  ;; amount_t x4(123456.0);
  ;; amount_t x5("123456.0");
  ;; amount_t x6(123456.0F);

  ;; assert-true(x1 == 123456L);
  ;; assert-true(x1 != x2);
  ;; assert-true(x1 == (x2 - x3));
  ;; assert-true(x1 == x4);
  ;; assert-true(x4 == x5);
  ;; assert-true(x4 == x6);

  ;; assert-true(x1 == 123456L);
  ;; assert-true(123456L == x1);
  ;; assert-true(x1 == 123456UL);
  ;; assert-true(123456UL == x1);
  ;; assert-true(x1 == 123456.0);
  ;; assert-true(123456.0 == x1);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  )

(define-test commodity-equality ()
  ;; amount_t x0;
  ;; amount_t x1;
  ;; amount_t x2;
  ;; amount_t x3;
  ;; amount_t x4;
  ;; amount_t x5;
  ;; amount_t x6;
  ;; amount_t x7;
  ;; amount_t x8;
  ;; amount_t x9;
  ;; amount_t x10;

  ;; x1  = "$123.45";
  ;; x2  = "-$123.45";
  ;; x3  = "$-123.45";
  ;; x4  = "DM 123.45";
  ;; x5  = "-DM 123.45";
  ;; x6  = "DM -123.45";
  ;; x7  = "123.45 euro";
  ;; x8  = "-123.45 euro";
  ;; x9  = "123.45€";
  ;; x10 = "-123.45€";

  ;; assert-true(x0.is_null());
  ;; assert-condition(x0.is_zero(), amount_error);
  ;; assert-condition(x0.is_realzero(), amount_error);
  ;; assert-condition(assert(x0.sign() == 0), amount_error);
  ;; assert-condition(assert(x0.compare(x1) < 0), amount_error);
  ;; assert-condition(assert(x0.compare(x2) > 0), amount_error);
  ;; assert-condition(assert(x0.compare(x0) == 0), amount_error);

  ;; assert-true(x1 != x2);
  ;; assert-true(x1 != x4);
  ;; assert-true(x1 != x7);
  ;; assert-true(x1 != x9);
  ;; assert-true(x2 == x3);
  ;; assert-true(x4 != x5);
  ;; assert-true(x5 == x6);
  ;; assert-true(x7 == - x8);
  ;; assert-true(x9 == - x10);

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  )

(define-test comparisons ()
  ;; amount_t x0;
  ;; amount_t x1(-123L);
  ;; amount_t x2(123L);
  ;; amount_t x3(-123.45);
  ;; amount_t x4(123.45);
  ;; amount_t x5("-123.45");
  ;; amount_t x6("123.45");

  ;; assert-condition(x0 > x1, amount_error);
  ;; assert-condition(x0 < x2, amount_error);
  ;; assert-condition(x0 > x3, amount_error);
  ;; assert-condition(x0 < x4, amount_error);
  ;; assert-condition(x0 > x5, amount_error);
  ;; assert-condition(x0 < x6, amount_error);

  ;; assert-true(x1 > x3);
  ;; assert-true(x3 <= x5);
  ;; assert-true(x3 >= x5);
  ;; assert-true(x3 < x1);
  ;; assert-true(x3 < x4);

  ;; assert-true(x1 < 100L);
  ;; assert-true(100L > x1);
  ;; assert-true(x1 < 100UL);
  ;; assert-true(100UL > x1);
  ;; assert-true(x1 < 100.0);
  ;; assert-true(100.0 > x1);

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  )

(define-test commodity-comparisons ()
  ;; amount_t x1("$-123");
  ;; amount_t x2("$123.00");
  ;; amount_t x3(internalAmount("$-123.4544"));
  ;; amount_t x4(internalAmount("$123.4544"));
  ;; amount_t x5("$-123.45");
  ;; amount_t x6("$123.45");
  ;; amount_t x7("DM 123.45");

  ;; assert-true(x1 > x3);
  ;; assert-true(x3 <= x5);
  ;; assert-true(x3 < x5);
  ;; assert-true(x3 <= x5);
  ;; assert-false(x3 == x5);
  ;; assert-true(x3 < x1);
  ;; assert-true(x3 < x4);
  ;; assert-false(x6 == x7);
  ;; assert-condition(x6 < x7, amount_error);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  )

(define-test integer-addition ()
  ;; amount_t x0;
  ;; amount_t x1(123L);
  ;; amount_t y1(456L);

  ;; assert-value-equal(amount_t(579L), x1 + y1);
  ;; assert-value-equal(amount_t(579L), x1 + 456L);
  ;; assert-value-equal(amount_t(579L), 456L + x1);

  ;; x1 += amount_t(456L);
  ;; assert-value-equal(amount_t(579L), x1);
  ;; x1 += 456L;
  ;; assert-value-equal(amount_t(1035L), x1);

  ;; amount_t x4("123456789123456789123456789");

  ;; assert-value-equal(amount_t("246913578246913578246913578"), x4 + x4);

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x4);
  )

(define-test fractional-addition ()
  ;; amount_t x1(123.123);
  ;; amount_t y1(456.456);

  ;; assert-value-equal(amount_t(579.579), x1 + y1);
  ;; assert-value-equal(amount_t(579.579), x1 + 456.456);
  ;; assert-value-equal(amount_t(579.579), 456.456 + x1);

  ;; x1 += amount_t(456.456);
  ;; assert-value-equal(amount_t(579.579), x1);
  ;; x1 += 456.456;
  ;; assert-value-equal(amount_t(1036.035), x1);
  ;; x1 += 456L;
  ;; assert-value-equal(amount_t(1492.035), x1);

  ;; amount_t x2("123456789123456789.123456789123456789");

  ;; assert-value-equal(amount_t("246913578246913578.246913578246913578"), x2 + x2);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x2);
  )

(define-test commodity-addition ()
  ;; amount_t x0;
  ;; amount_t x1("$123.45");
  ;; amount_t x2(internalAmount("$123.456789"));
  ;; amount_t x3("DM 123.45");
  ;; amount_t x4("123.45 euro");
  ;; amount_t x5("123.45€");
  ;; amount_t x6("123.45");

  ;; assert-value-equal(amount_t("$246.90"), x1 + x1);
  ;; assert-value-not-equal(amount_t("$246.91"), x1 + x2);
  ;; assert-value-equal(internalAmount("$246.906789"), x1 + x2);

  ;; // Converting to string drops internal precision
  ;; assert-value-equal(string("$246.90"), (x1 + x1).to_string());
  ;; assert-value-equal(string("$246.91"), (x1 + x2).to_string());

  ;; assert-condition(x1 + x0, amount_error);
  ;; assert-condition(x0 + x1, amount_error);
  ;; assert-condition(x0 + x0, amount_error);
  ;; assert-condition(x1 + x3, amount_error);
  ;; assert-condition(x1 + x4, amount_error);
  ;; assert-condition(x1 + x5, amount_error);
  ;; assert-condition(x1 + x6, amount_error);
  ;; assert-condition(x1 + 123.45, amount_error);
  ;; assert-condition(x1 + 123L, amount_error);

  ;; assert-value-equal(amount_t("DM 246.90"), x3 + x3);
  ;; assert-value-equal(amount_t("246.90 euro"), x4 + x4);
  ;; assert-value-equal(amount_t("246.90€"), x5 + x5);

  ;; assert-value-equal(string("DM 246.90"), (x3 + x3).to_string());
  ;; assert-value-equal(string("246.90 euro"), (x4 + x4).to_string());
  ;; assert-value-equal(string("246.90€"), (x5 + x5).to_string());

  ;; x1 += amount_t("$456.45");
  ;; assert-value-equal(amount_t("$579.90"), x1);
  ;; x1 += amount_t("$456.45");
  ;; assert-value-equal(amount_t("$1036.35"), x1);
  ;; x1 += amount_t("$456");
  ;; assert-value-equal(amount_t("$1492.35"), x1);

  ;; amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  ;; assert-value-equal(internalAmount("$246913578246913578.246913578246913578"), x7 + x7);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  )

(define-test integer-subtraction ()
  ;; amount_t x1(123L);
  ;; amount_t y1(456L);

  ;; assert-value-equal(amount_t(333L), y1 - x1);
  ;; assert-value-equal(amount_t(-333L), x1 - y1);
  ;; assert-value-equal(amount_t(23L), x1 - 100L);
  ;; assert-value-equal(amount_t(-23L), 100L - x1);

  ;; x1 -= amount_t(456L);
  ;; assert-value-equal(amount_t(-333L), x1);
  ;; x1 -= 456L;
  ;; assert-value-equal(amount_t(-789L), x1);

  ;; amount_t x4("123456789123456789123456789");
  ;; amount_t y4("8238725986235986");

  ;; assert-value-equal(amount_t("123456789115218063137220803"), x4 - y4);
  ;; assert-value-equal(amount_t("-123456789115218063137220803"), y4 - x4);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x4);
  ;; assert-valid(y4);
  )

(define-test fractional-subtraction ()
  ;; amount_t x1(123.123);
  ;; amount_t y1(456.456);

  ;; assert-value-equal(amount_t(-333.333), x1 - y1);
  ;; assert-value-equal(amount_t(333.333), y1 - x1);

  ;; x1 -= amount_t(456.456);
  ;; assert-value-equal(amount_t(-333.333), x1);
  ;; x1 -= 456.456;
  ;; assert-value-equal(amount_t(-789.789), x1);
  ;; x1 -= 456L;
  ;; assert-value-equal(amount_t(-1245.789), x1);

  ;; amount_t x2("123456789123456789.123456789123456789");
  ;; amount_t y2("9872345982459.248974239578");

  ;; assert-value-equal(amount_t("123446916777474329.874482549545456789"), x2 - y2);
  ;; assert-value-equal(amount_t("-123446916777474329.874482549545456789"), y2 - x2);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x2);
  ;; assert-valid(y2);
  )

(define-test commodity-subtraction ()
  ;; amount_t x0;
  ;; amount_t x1("$123.45");
  ;; amount_t x2(internalAmount("$123.456789"));
  ;; amount_t x3("DM 123.45");
  ;; amount_t x4("123.45 euro");
  ;; amount_t x5("123.45€");
  ;; amount_t x6("123.45");

  ;; assert-value-not-equal(amount_t(), x1 - x1);
  ;; assert-value-equal(amount_t("$0"), x1 - x1);
  ;; assert-value-equal(amount_t("$23.45"), x1 - amount_t("$100.00"));
  ;; assert-value-equal(amount_t("$-23.45"), amount_t("$100.00") - x1);
  ;; assert-value-not-equal(amount_t("$-0.01"), x1 - x2);
  ;; assert-value-equal(internalAmount("$-0.006789"), x1 - x2);

  ;; // Converting to string drops internal precision.  If an amount is
  ;; // zero, it drops the commodity as well.
  ;; assert-value-equal(string("$0.00"), (x1 - x1).to_string());
  ;; assert-value-equal(string("$-0.01"), (x1 - x2).to_string());

  ;; assert-condition(x1 - x0, amount_error);
  ;; assert-condition(x0 - x1, amount_error);
  ;; assert-condition(x0 - x0, amount_error);
  ;; assert-condition(x1 - x3, amount_error);
  ;; assert-condition(x1 - x4, amount_error);
  ;; assert-condition(x1 - x5, amount_error);
  ;; assert-condition(x1 - x6, amount_error);
  ;; assert-condition(x1 - 123.45, amount_error);
  ;; assert-condition(x1 - 123L, amount_error);

  ;; assert-value-equal(amount_t("DM 0.00"), x3 - x3);
  ;; assert-value-equal(amount_t("DM 23.45"), x3 - amount_t("DM 100.00"));
  ;; assert-value-equal(amount_t("DM -23.45"), amount_t("DM 100.00") - x3);
  ;; assert-value-equal(amount_t("0.00 euro"), x4 - x4);
  ;; assert-value-equal(amount_t("23.45 euro"), x4 - amount_t("100.00 euro"));
  ;; assert-value-equal(amount_t("-23.45 euro"), amount_t("100.00 euro") - x4);
  ;; assert-value-equal(amount_t("0.00€"), x5 - x5);
  ;; assert-value-equal(amount_t("23.45€"), x5 - amount_t("100.00€"));
  ;; assert-value-equal(amount_t("-23.45€"), amount_t("100.00€") - x5);

  ;; assert-value-equal(string("DM 0.00"), (x3 - x3).to_string());
  ;; assert-value-equal(string("DM 23.45"), (x3 - amount_t("DM 100.00")).to_string());
  ;; assert-value-equal(string("DM -23.45"), (amount_t("DM 100.00") - x3).to_string());
  ;; assert-value-equal(string("0.00 euro"), (x4 - x4).to_string());
  ;; assert-value-equal(string("23.45 euro"), (x4 - amount_t("100.00 euro")).to_string());
  ;; assert-value-equal(string("-23.45 euro"), (amount_t("100.00 euro") - x4).to_string());
  ;; assert-value-equal(string("0.00€"), (x5 - x5).to_string());
  ;; assert-value-equal(string("23.45€"), (x5 - amount_t("100.00€")).to_string());
  ;; assert-value-equal(string("-23.45€"), (amount_t("100.00€") - x5).to_string());

  ;; x1 -= amount_t("$456.45");
  ;; assert-value-equal(amount_t("$-333.00"), x1);
  ;; x1 -= amount_t("$456.45");
  ;; assert-value-equal(amount_t("$-789.45"), x1);
  ;; x1 -= amount_t("$456");
  ;; assert-value-equal(amount_t("$-1245.45"), x1);

  ;; amount_t x7(internalAmount("$123456789123456789.123456789123456789"));
  ;; amount_t x8(internalAmount("$2354974984698.98459845984598"));

  ;; assert-value-equal(internalAmount("$123454434148472090.138858329277476789"), x7 - x8);
  ;; assert-value-equal(string("$123454434148472090.138858329277476789"), (x7 - x8).to_string());
  ;; assert-value-equal(string("$123454434148472090.14"), (amount_t("$1.00") * (x7 - x8)).to_string()) ;
  ;; assert-value-equal(internalAmount("$-123454434148472090.138858329277476789"), x8 - x7);
  ;; assert-value-equal(string("$-123454434148472090.138858329277476789"), (x8 - x7).to_string());
  ;; assert-value-equal(string("$-123454434148472090.14"), (amount_t("$1.00") * (x8 - x7)).to_string()) ;

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  )

(define-test integer-multiplication ()
  ;; amount_t x1(123L);
  ;; amount_t y1(456L);

  ;; assert-value-equal(amount_t(0L), x1 * 0L);
  ;; assert-value-equal(amount_t(0L), amount_t(0L) * x1);
  ;; assert-value-equal(amount_t(0L), 0L * x1);
  ;; assert-value-equal(x1, x1 * 1L);
  ;; assert-value-equal(x1, amount_t(1L) * x1);
  ;; assert-value-equal(x1, 1L * x1);
  ;; assert-value-equal(- x1, x1 * -1L);
  ;; assert-value-equal(- x1, amount_t(-1L) * x1);
  ;; assert-value-equal(- x1, -1L * x1);
  ;; assert-value-equal(amount_t(56088L), x1 * y1);
  ;; assert-value-equal(amount_t(56088L), y1 * x1);
  ;; assert-value-equal(amount_t(56088L), x1 * 456L);
  ;; assert-value-equal(amount_t(56088L), amount_t(456L) * x1);
  ;; assert-value-equal(amount_t(56088L), 456L * x1);

  ;; x1 *= amount_t(123L);
  ;; assert-value-equal(amount_t(15129L), x1);
  ;; x1 *= 123L;
  ;; assert-value-equal(amount_t(1860867L), x1);

  ;; amount_t x4("123456789123456789123456789");

  ;; assert-value-equal(amount_t("15241578780673678546105778281054720515622620750190521"), x4 * x4);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x4);
  )

(define-test fractional-multiplication ()
  ;; amount_t x1(123.123);
  ;; amount_t y1(456.456);

  ;; assert-value-equal(amount_t(0L), x1 * 0L);
  ;; assert-value-equal(amount_t(0L), amount_t(0L) * x1);
  ;; assert-value-equal(amount_t(0L), 0L * x1);
  ;; assert-value-equal(x1, x1 * 1L);
  ;; assert-value-equal(x1, amount_t(1L) * x1);
  ;; assert-value-equal(x1, 1L * x1);
  ;; assert-value-equal(- x1, x1 * -1L);
  ;; assert-value-equal(- x1, amount_t(-1L) * x1);
  ;; assert-value-equal(- x1, -1L * x1);
  ;; assert-value-equal(amount_t("56200.232088"), x1 * y1);
  ;; assert-value-equal(amount_t("56200.232088"), y1 * x1);
  ;; assert-value-equal(amount_t("56200.232088"), x1 * 456.456);
  ;; assert-value-equal(amount_t("56200.232088"), amount_t(456.456) * x1);
  ;; assert-value-equal(amount_t("56200.232088"), 456.456 * x1);

  ;; x1 *= amount_t(123.123);
  ;; assert-value-equal(amount_t("15159.273129"), x1);
  ;; x1 *= 123.123;
  ;; assert-value-equal(amount_t("1866455.185461867"), x1);
  ;; x1 *= 123L;
  ;; assert-value-equal(amount_t("229573987.811809641"), x1);

  ;; amount_t x2("123456789123456789.123456789123456789");

  ;; assert-value-equal(amount_t("15241578780673678546105778311537878.046486820281054720515622620750190521"), x2 * x2);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x2);
  )

(define-test commodity-multiplication ()
  ;; amount_t x0;
  ;; amount_t x1("$123.12");
  ;; amount_t y1("$456.45");
  ;; amount_t x2(internalAmount("$123.456789"));
  ;; amount_t x3("DM 123.45");
  ;; amount_t x4("123.45 euro");
  ;; amount_t x5("123.45€");

  ;; assert-value-equal(amount_t("$0.00"), x1 * 0L);
  ;; assert-value-equal(amount_t("$0.00"), 0L * x1);
  ;; assert-value-equal(x1, x1 * 1L);
  ;; assert-value-equal(x1, 1L * x1);
  ;; assert-value-equal(- x1, x1 * -1L);
  ;; assert-value-equal(- x1, -1L * x1);
  ;; assert-value-equal(internalAmount("$56198.124"), x1 * y1);
  ;; assert-value-equal(string("$56198.12"), (x1 * y1).to_string());
  ;; assert-value-equal(internalAmount("$56198.124"), y1 * x1);
  ;; assert-value-equal(string("$56198.12"), (y1 * x1).to_string());

  ;; // Internal amounts retain their precision, even when being
  ;; // converted to strings
  ;; assert-value-equal(internalAmount("$15199.99986168"), x1 * x2);
  ;; assert-value-equal(internalAmount("$15199.99986168"), x2 * x1);
  ;; assert-value-equal(string("$15200.00"), (x1 * x2).to_string());
  ;; assert-value-equal(string("$15199.99986168"), (x2 * x1).to_string());

  ;; assert-condition(x1 * x0, amount_error);
  ;; assert-condition(x0 * x1, amount_error);
  ;; assert-condition(x0 * x0, amount_error);
  ;; assert-condition(x1 * x3, amount_error);
  ;; assert-condition(x1 * x4, amount_error);
  ;; assert-condition(x1 * x5, amount_error);

  ;; x1 *= amount_t("123.12");
  ;; assert-value-equal(internalAmount("$15158.5344"), x1);
  ;; assert-value-equal(string("$15158.53"), x1.to_string());
  ;; x1 *= 123.12;
  ;; assert-value-equal(internalAmount("$1866318.755328"), x1);
  ;; assert-value-equal(string("$1866318.76"), x1.to_string());
  ;; x1 *= 123L;
  ;; assert-value-equal(internalAmount("$229557206.905344"), x1);
  ;; assert-value-equal(string("$229557206.91"), x1.to_string());

  ;; amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  ;; assert-value-equal(internalAmount("$15241578780673678546105778311537878.046486820281054720515622620750190521"), x7 * x7);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x7);
  )

(define-test integer-division ()
  ;; amount_t x1(123L);
  ;; amount_t y1(456L);

  ;; assert-condition(x1 / 0L, amount_error);
  ;; assert-value-equal(amount_t(0L), amount_t(0L) / x1);
  ;; assert-value-equal(amount_t(0L), 0L / x1);
  ;; assert-value-equal(x1, x1 / 1L);
  ;; assert-value-equal(amount_t("0.008130"), amount_t(1L) / x1);
  ;; assert-value-equal(amount_t("0.008130"), 1L / x1);
  ;; assert-value-equal(- x1, x1 / -1L);
  ;; assert-value-equal(- amount_t("0.008130"), amount_t(-1L) / x1);
  ;; assert-value-equal(- amount_t("0.008130"), -1L / x1);
  ;; assert-value-equal(amount_t("0.269737"), x1 / y1);
  ;; assert-value-equal(amount_t("3.707317"), y1 / x1);
  ;; assert-value-equal(amount_t("0.269737"), x1 / 456L);
  ;; assert-value-equal(amount_t("3.707317"), amount_t(456L) / x1);
  ;; assert-value-equal(amount_t("3.707317"), 456L / x1);

  ;; x1 /= amount_t(456L);
  ;; assert-value-equal(amount_t("0.269737"), x1);
  ;; x1 /= 456L;
  ;; assert-value-equal(amount_t("0.00059152850877193"), x1);

  ;; amount_t x4("123456789123456789123456789");
  ;; amount_t y4("56");

  ;; assert-value-equal(amount_t(1L), x4 / x4);
  ;; assert-value-equal(amount_t("2204585520061728377204585.517857"), x4 / y4);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x4);
  ;; assert-valid(y4);
  )

(define-test fractional-division ()
  ;; amount_t x1(123.123);
  ;; amount_t y1(456.456);

  ;; assert-condition(x1 / 0L, amount_error);
  ;; assert-value-equal(amount_t("0.008121959"), amount_t(1.0) / x1);
  ;; assert-value-equal(amount_t("0.008121959"), 1.0 / x1);
  ;; assert-value-equal(x1, x1 / 1.0);
  ;; assert-value-equal(amount_t("0.008121959"), amount_t(1.0) / x1);
  ;; assert-value-equal(amount_t("0.008121959"), 1.0 / x1);
  ;; assert-value-equal(- x1, x1 / -1.0);
  ;; assert-value-equal(- amount_t("0.008121959"), amount_t(-1.0) / x1);
  ;; assert-value-equal(- amount_t("0.008121959"), -1.0 / x1);
  ;; assert-value-equal(amount_t("0.269736842105263"), x1 / y1);
  ;; assert-value-equal(amount_t("3.707317073170732"), y1 / x1);
  ;; assert-value-equal(amount_t("0.269736842105263"), x1 / 456.456);
  ;; assert-value-equal(amount_t("3.707317073170732"), amount_t(456.456) / x1);
  ;; assert-value-equal(amount_t("3.707317073170732"), 456.456 / x1);

  ;; x1 /= amount_t(456.456);
  ;; assert-value-equal(amount_t("0.269736842105263"), x1);
  ;; x1 /= 456.456;
  ;; assert-value-equal(amount_t("0.000590937225286255411255411255411255411"), x1);
  ;; x1 /= 456L;
  ;; assert-value-equal(amount_t("0.000001295914967733016252753094858358016252192982456140350877192982456140350877192982"), x1);

  ;; amount_t x4("1234567891234567.89123456789");
  ;; amount_t y4("56.789");

  ;; assert-value-equal(amount_t(1.0), x4 / x4);
  ;; assert-value-equal(amount_t("21739560323910.7554497273748437197344556164046"), x4 / y4);

  ;; assert-valid(x1);
  ;; assert-valid(y1);
  ;; assert-valid(x4);
  ;; assert-valid(y4);
  )

(define-test commodity-division ()
  (let ((x1 (amount "$123.12"))
	(y1 (amount "$456.45"))
	(x2 (exact-amount "$123.456789"))
	(x3 (amount "DM 123.45"))
	(x4 (amount "123.45 euro"))
	;; (x5 (amount "123.45€"))
	)

    (assert-condition 'amount-error (divide x1 0))
    (assert-true (value-zerop (divide 0 x1)))
    ;; assert-value-equal(amount_t("$0.00"), 0L / x1);
    ;; assert-value-equal(x1, x1 / 1L);
    ;; assert-value-equal(internalAmount("$0.00812216"), 1L / x1);
    ;; assert-value-equal(- x1, x1 / -1L);
    ;; assert-value-equal(internalAmount("$-0.00812216"), -1L / x1);
    ;; assert-value-equal(internalAmount("$0.26973382"), x1 / y1);
    ;; assert-value-equal(string("$0.27"), (x1 / y1).to_string());
    ;; assert-value-equal(internalAmount("$3.70735867"), y1 / x1);
    ;; assert-value-equal(string("$3.71"), (y1 / x1).to_string());

    ;; // Internal amounts retain their precision, even when being
    ;; // converted to strings
    ;; assert-value-equal(internalAmount("$0.99727201"), x1 / x2);
    ;; assert-value-equal(internalAmount("$1.00273545321637426901"), x2 / x1);
    ;; assert-value-equal(string("$1.00"), (x1 / x2).to_string());
    ;; assert-value-equal(string("$1.00273545321637426901"), (x2 / x1).to_string());

    ;; assert-condition(x1 / x0, amount_error);
    ;; assert-condition(x0 / x1, amount_error);
    ;; assert-condition(x0 / x0, amount_error);
    ;; assert-condition(x1 / x3, amount_error);
    ;; assert-condition(x1 / x4, amount_error);
    ;; assert-condition(x1 / x5, amount_error);

    ;; x1 /= amount_t("123.12");
    ;; assert-value-equal(internalAmount("$1.00"), x1);
    ;; assert-value-equal(string("$1.00"), x1.to_string());
    ;; x1 /= 123.12;
    ;; assert-value-equal(internalAmount("$0.00812216"), x1);
    ;; assert-value-equal(string("$0.01"), x1.to_string());
    ;; x1 /= 123L;
    ;; assert-value-equal(internalAmount("$0.00006603"), x1);
    ;; assert-value-equal(string("$0.00"), x1.to_string());

    ;; amount_t x6(internalAmount("$237235987235987.98723987235978"));
    ;; amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

    ;; assert-value-equal(amount_t("$1"), x7 / x7);
    ;; assert-value-equal(internalAmount("$0.0019216115121765559608381226612019501046413574469262"), x6 / x7);
    ;; assert-value-equal(internalAmount("$520.39654928343335571379527154924040947271699678158689736256"), x7 / x6);

    ;; assert-valid(x1);
    ;; assert-valid(x2);
    ;; assert-valid(x3);
    ;; assert-valid(x4);
    ;; assert-valid(x5);
    ;; assert-valid(x6);
    ;; assert-valid(x7);
    ))

(define-test negation ()
  ;; amount_t x0;
  ;; amount_t x1(-123456L);
  ;; amount_t x3(-123.456);
  ;; amount_t x5("-123456");
  ;; amount_t x6("-123.456");
  ;; amount_t x7(string("-123456"));
  ;; amount_t x8(string("-123.456"));
  ;; amount_t x9(- x3);

  ;; assert-condition(x0.negate(), amount_error);
  ;; assert-value-equal(x5, x1);
  ;; assert-value-equal(x7, x1);
  ;; assert-value-equal(x6, x3);
  ;; assert-value-equal(x8, x3);
  ;; assert-value-equal(- x6, x9);
  ;; assert-value-equal(x3.negate(), x9);

  ;; amount_t x10(x9.negate());

  ;; assert-value-equal(x3, x10);

  ;; assert-valid(x1);
  ;; assert-valid(x3);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  )

(define-test commodity-negation ()
  ;; amount_t x1("$123.45");
  ;; amount_t x2("-$123.45");
  ;; amount_t x3("$-123.45");
  ;; amount_t x4("DM 123.45");
  ;; amount_t x5("-DM 123.45");
  ;; amount_t x6("DM -123.45");
  ;; amount_t x7("123.45 euro");
  ;; amount_t x8("-123.45 euro");
  ;; amount_t x9("123.45€");
  ;; amount_t x10("-123.45€");

  ;; assert-value-equal(amount_t("$-123.45"), - x1);
  ;; assert-value-equal(amount_t("$123.45"), - x2);
  ;; assert-value-equal(amount_t("$123.45"), - x3);
  ;; assert-value-equal(amount_t("DM -123.45"), - x4);
  ;; assert-value-equal(amount_t("DM 123.45"), - x5);
  ;; assert-value-equal(amount_t("DM 123.45"), - x6);
  ;; assert-value-equal(amount_t("-123.45 euro"), - x7);
  ;; assert-value-equal(amount_t("123.45 euro"), - x8);
  ;; assert-value-equal(amount_t("-123.45€"), - x9);
  ;; assert-value-equal(amount_t("123.45€"), - x10);

  ;; assert-value-equal(amount_t("$-123.45"), x1.negate());
  ;; assert-value-equal(amount_t("$123.45"), x2.negate());
  ;; assert-value-equal(amount_t("$123.45"), x3.negate());

  ;; assert-value-equal(string("$-123.45"), (- x1).to_string());
  ;; assert-value-equal(string("$123.45"), (- x2).to_string());
  ;; assert-value-equal(string("$123.45"), (- x3).to_string());
  ;; assert-value-equal(string("DM -123.45"), (- x4).to_string());
  ;; assert-value-equal(string("DM 123.45"), (- x5).to_string());
  ;; assert-value-equal(string("DM 123.45"), (- x6).to_string());
  ;; assert-value-equal(string("-123.45 euro"), (- x7).to_string());
  ;; assert-value-equal(string("123.45 euro"), (- x8).to_string());
  ;; assert-value-equal(string("-123.45€"), (- x9).to_string());
  ;; assert-value-equal(string("123.45€"), (- x10).to_string());

  ;; assert-value-equal(amount_t("$-123.45"), x1.negate());
  ;; assert-value-equal(amount_t("$123.45"), x2.negate());
  ;; assert-value-equal(amount_t("$123.45"), x3.negate());

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  ;; assert-valid(x6);
  ;; assert-valid(x7);
  ;; assert-valid(x8);
  ;; assert-valid(x9);
  ;; assert-valid(x10);
  )

(define-test abs ()
  ;; amount_t x0;
  ;; amount_t x1(-1234L);
  ;; amount_t x2(1234L);

  ;; assert-condition(x0.abs(), amount_error);
  ;; assert-value-equal(amount_t(1234L), x1.abs());
  ;; assert-value-equal(amount_t(1234L), x2.abs());

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  )

(define-test commodity-abs ()
  ;; amount_t x1("$-1234.56");
  ;; amount_t x2("$1234.56");

  ;; assert-value-equal(amount_t("$1234.56"), x1.abs());
  ;; assert-value-equal(amount_t("$1234.56"), x2.abs());

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  )

(define-test fractional-round ()
  ;; amount_t x0;
  ;; amount_t x1("1234.567890");

  ;; assert-condition(x0.precision(), amount_error);
  ;; assert-condition(x0.round(), amount_error);
  ;; assert-condition(x0.round(2), amount_error);
  ;; assert-condition(x0.unround(), amount_error);
  ;; assert-value-equal(amount_t::precision_t(6), x1.precision());

  ;; amount_t x1b(x1.unround());

  ;; assert-value-equal(x1b.precision(), x1b.unround().precision());

  ;; amount_t y7(x1.round(7));
  ;; amount_t y6(x1.round(6));
  ;; amount_t y5(x1.round(5));
  ;; amount_t y4(x1.round(4));
  ;; amount_t y3(x1.round(3));
  ;; amount_t y2(x1.round(2));
  ;; amount_t y1(x1.round(1));
  ;; amount_t y0(x1.round(0));

  ;; assert-value-equal(amount_t::precision_t(6), y7.precision());
  ;; assert-value-equal(amount_t::precision_t(6), y6.precision());
  ;; assert-value-equal(amount_t::precision_t(5), y5.precision());
  ;; assert-value-equal(amount_t::precision_t(4), y4.precision());
  ;; assert-value-equal(amount_t::precision_t(3), y3.precision());
  ;; assert-value-equal(amount_t::precision_t(2), y2.precision());
  ;; assert-value-equal(amount_t::precision_t(1), y1.precision());
  ;; assert-value-equal(amount_t::precision_t(0), y0.precision());

  ;; assert-value-equal(amount_t("1234.56789"), y7);
  ;; assert-value-equal(amount_t("1234.56789"), y6);
  ;; assert-value-equal(amount_t("1234.56789"), y5);
  ;; assert-value-equal(amount_t("1234.5679"), y4);
  ;; assert-value-equal(amount_t("1234.568"), y3);
  ;; assert-value-equal(amount_t("1234.57"), y2);
  ;; assert-value-equal(amount_t("1234.6"), y1);
  ;; assert-value-equal(amount_t("1235"), y0);

  ;; amount_t x2("9876.543210");

  ;; assert-value-equal(amount_t("9876.543210"), x2.round(6));
  ;; assert-value-equal(amount_t("9876.54321"), x2.round(5));
  ;; assert-value-equal(amount_t("9876.5432"), x2.round(4));
  ;; assert-value-equal(amount_t("9876.543"), x2.round(3));
  ;; assert-value-equal(amount_t("9876.54"), x2.round(2));
  ;; assert-value-equal(amount_t("9876.5"), x2.round(1));
  ;; assert-value-equal(amount_t("9877"), x2.round(0));

  ;; amount_t x3("-1234.567890");

  ;; assert-value-equal(amount_t("-1234.56789"), x3.round(6));
  ;; assert-value-equal(amount_t("-1234.56789"), x3.round(5));
  ;; assert-value-equal(amount_t("-1234.5679"), x3.round(4));
  ;; assert-value-equal(amount_t("-1234.568"), x3.round(3));
  ;; assert-value-equal(amount_t("-1234.57"), x3.round(2));
  ;; assert-value-equal(amount_t("-1234.6"), x3.round(1));
  ;; assert-value-equal(amount_t("-1235"), x3.round(0));

  ;; amount_t x4("-9876.543210");

  ;; assert-value-equal(amount_t("-9876.543210"), x4.round(6));
  ;; assert-value-equal(amount_t("-9876.54321"), x4.round(5));
  ;; assert-value-equal(amount_t("-9876.5432"), x4.round(4));
  ;; assert-value-equal(amount_t("-9876.543"), x4.round(3));
  ;; assert-value-equal(amount_t("-9876.54"), x4.round(2));
  ;; assert-value-equal(amount_t("-9876.5"), x4.round(1));
  ;; assert-value-equal(amount_t("-9877"), x4.round(0));

  ;; amount_t x5("0.0000000000000000000000000000000000001");

  ;; assert-value-equal(amount_t("0.0000000000000000000000000000000000001"), x5.round(37));
  ;; assert-value-equal(amount_t(0L), x5.round(36));

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  )

(define-test commodity-round ()
  ;; amount_t x1(internalAmount("$1234.567890"));

  ;; assert-value-equal(internalAmount("$1234.56789"), x1.round(6));
  ;; assert-value-equal(internalAmount("$1234.56789"), x1.round(5));
  ;; assert-value-equal(internalAmount("$1234.5679"), x1.round(4));
  ;; assert-value-equal(internalAmount("$1234.568"), x1.round(3));
  ;; assert-value-equal(amount_t("$1234.57"), x1.round(2));
  ;; assert-value-equal(amount_t("$1234.6"), x1.round(1));
  ;; assert-value-equal(amount_t("$1235"), x1.round(0));

  ;; amount_t x2(internalAmount("$9876.543210"));

  ;; assert-value-equal(internalAmount("$9876.543210"), x2.round(6));
  ;; assert-value-equal(internalAmount("$9876.54321"), x2.round(5));
  ;; assert-value-equal(internalAmount("$9876.5432"), x2.round(4));
  ;; assert-value-equal(internalAmount("$9876.543"), x2.round(3));
  ;; assert-value-equal(amount_t("$9876.54"), x2.round(2));
  ;; assert-value-equal(amount_t("$9876.5"), x2.round(1));
  ;; assert-value-equal(amount_t("$9877"), x2.round(0));

  ;; amount_t x3(internalAmount("$-1234.567890"));

  ;; assert-value-equal(internalAmount("$-1234.56789"), x3.round(6));
  ;; assert-value-equal(internalAmount("$-1234.56789"), x3.round(5));
  ;; assert-value-equal(internalAmount("$-1234.5679"), x3.round(4));
  ;; assert-value-equal(internalAmount("$-1234.568"), x3.round(3));
  ;; assert-value-equal(amount_t("$-1234.57"), x3.round(2));
  ;; assert-value-equal(amount_t("$-1234.6"), x3.round(1));
  ;; assert-value-equal(amount_t("$-1235"), x3.round(0));

  ;; amount_t x4(internalAmount("$-9876.543210"));

  ;; assert-value-equal(internalAmount("$-9876.543210"), x4.round(6));
  ;; assert-value-equal(internalAmount("$-9876.54321"), x4.round(5));
  ;; assert-value-equal(internalAmount("$-9876.5432"), x4.round(4));
  ;; assert-value-equal(internalAmount("$-9876.543"), x4.round(3));
  ;; assert-value-equal(amount_t("$-9876.54"), x4.round(2));
  ;; assert-value-equal(amount_t("$-9876.5"), x4.round(1));
  ;; assert-value-equal(amount_t("$-9877"), x4.round(0));

  ;; amount_t x5("$123.45");

  ;; x5 *= 100.12;

  ;; assert-value-equal(internalAmount("$12359.814"), x5);
  ;; assert-value-equal(string("$12359.81"), x5.to_string());
  ;; assert-value-equal(string("$12359.814"), x5.to_fullstring());
  ;; assert-value-equal(string("$12359.814"), x5.unround().to_string());

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x5);
  )

(define-test commodity-display-round ()
  ;; amount_t x1("$0.85");
  ;; amount_t x2("$0.1");

  ;; x1 *= 0.19;

  ;; assert-value-not-equal(amount_t("$0.16"), x1);
  ;; assert-value-equal(internalAmount("$0.1615"), x1);
  ;; assert-value-equal(string("$0.16"), x1.to_string());

  ;; assert-value-equal(amount_t("$0.10"), x2);
  ;; assert-value-not-equal(internalAmount("$0.101"), x2);
  ;; assert-value-equal(string("$0.10"), x2.to_string());

  ;; x1 *= 7L;

  ;; assert-value-not-equal(amount_t("$1.13"), x1);
  ;; assert-value-equal(internalAmount("$1.1305"), x1);
  ;; assert-value-equal(string("$1.13"), x1.to_string());
  )

(define-test reduction ()
  ;; amount_t x0;
  ;; amount_t x1("60s");
  ;; amount_t x2("600s");
  ;; amount_t x3("6000s");
  ;; amount_t x4("360000s");
  ;; amount_t x5("10m");		// 600s
  ;; amount_t x6("100m");		// 6000s
  ;; amount_t x7("1000m");		// 60000s
  ;; amount_t x8("10000m");	// 600000s
  ;; amount_t x9("10h");		// 36000s
  ;; amount_t x10("100h");		// 360000s
  ;; amount_t x11("1000h");	// 3600000s
  ;; amount_t x12("10000h");	// 36000000s

  ;; assert-condition(x0.reduce(), amount_error);
  ;; assert-condition(x0.unreduce(), amount_error);
  ;; assert-value-equal(x2, x5);
  ;; assert-value-equal(x3, x6);
  ;; assert-value-equal(x4, x10);
  ;; assert-value-equal(string("100.0h"), x4.unreduce().to_string());
  )

(define-test sign ()
  ;; amount_t x0;
  ;; amount_t x1("0.0000000000000000000000000000000000001");
  ;; amount_t x2("-0.0000000000000000000000000000000000001");
  ;; amount_t x3("1");
  ;; amount_t x4("-1");

  ;; assert-condition(x0.sign(), amount_error);
  ;; assert-true(x1.sign() > 0);
  ;; assert-true(x2.sign() < 0);
  ;; assert-true(x3.sign() > 0);
  ;; assert-true(x4.sign() < 0);

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  )

(define-test commodity-sign ()
  ;; amount_t x1(internalAmount("$0.0000000000000000000000000000000000001"));
  ;; amount_t x2(internalAmount("$-0.0000000000000000000000000000000000001"));
  ;; amount_t x3("$1");
  ;; amount_t x4("$-1");

  ;; assert-true(x1.sign() != 0);
  ;; assert-true(x2.sign() != 0);
  ;; assert-true(x3.sign() > 0);
  ;; assert-true(x4.sign() < 0);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  )

(define-test truth ()
  ;; amount_t x0;
  ;; amount_t x1("1234");
  ;; amount_t x2("1234.56");

  ;; assert-condition(assert(x0 ? 1 : 0), amount_error);

  ;; assert-true(x1);
  ;; assert-true(x2);

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  ;; assert-valid(x2);
  )

(define-test commodity-truth ()
  ;; amount_t x1("$1234");
  ;; amount_t x2("$1234.56");

  ;; if (x1)
  ;;   assert-true(true);
  ;; else
  ;;   assert-true(false);

  ;; if (x2)
  ;;   assert-true(true);
  ;; else
  ;;   assert-true(false);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  )

(define-test for-zero ()
  ;; amount_t x0;
  ;; amount_t x1("0.000000000000000000001");

  ;; assert-true(x1);
  ;; assert-condition(x0.is_zero(), amount_error);
  ;; assert-condition(x0.is_realzero(), amount_error);
  ;; assert-false(x1.is_zero());
  ;; assert-false(x1.is_realzero());

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  )

(define-test commodity-for-zero ()
  ;; amount_t x1(internalAmount("$0.000000000000000000001"));

  ;; assert-false(x1);
  ;; assert-true(x1.is_zero());
  ;; assert-false(x1.is_realzero());

  ;; assert-valid(x1);
  )

(define-test integer-conversion ()
  ;; amount_t x0;
  ;; amount_t x1(123456L);
  ;; amount_t x2("12345682348723487324");

  ;; assert-condition(x0.to_long(), amount_error);
  ;; assert-condition(x0.to_double(), amount_error);
  ;; assert-false(x2.fits_in_long());
  ;; assert-value-equal(123456L, x1.to_long());
  ;; assert-value-equal(123456.0, x1.to_double());
  ;; assert-value-equal(string("123456"), x1.to_string());
  ;; assert-value-equal(string("123456"), x1.quantity_string());

  ;; assert-valid(x1);
  )

(define-test fractional-conversion ()
  ;; amount_t x1(1234.56);
  ;; amount_t x2("1234.5683787634678348734");

  ;; assert-condition(x1.to_long(), amount_error); // loses precision
  ;; assert-condition(x2.to_double(), amount_error); // loses precision
  ;; assert-false(x2.fits_in_double());
  ;; assert-value-equal(1234L, x1.to_long(true));
  ;; assert-value-equal(1234.56, x1.to_double());
  ;; assert-value-equal(string("1234.56"), x1.to_string());
  ;; assert-value-equal(string("1234.56"), x1.quantity_string());

  ;; assert-valid(x1);
  )

(define-test commodity-conversion ()
  ;; amount_t x1("$1234.56");

  ;; assert-condition(x1.to_long(), amount_error); // loses precision
  ;; assert-value-equal(1234L, x1.to_long(true));
  ;; assert-value-equal(1234.56, x1.to_double());
  ;; assert-value-equal(string("$1234.56"), x1.to_string());
  ;; assert-value-equal(string("1234.56"), x1.quantity_string());

  ;; assert-valid(x1);
  )

(define-test printing ()
  ;; amount_t x0;
  ;; amount_t x1("982340823.380238098235098235098235098");

  ;; {
  ;;   std::ostringstream bufstr;
  ;;   assert-condition(bufstr << x0, amount_error);
  ;; }

  ;; {
  ;;   std::ostringstream bufstr;
  ;;   bufstr << x1;

  ;;   assert-value-equal(std::string("982340823.380238098235098235098235098"), bufstr.str());
  ;; }

  ;; assert-valid(x0);
  ;; assert-valid(x1);
  )

(define-test commodity-printing ()
  ;; amount_t x1(internalAmount("$982340823.386238098235098235098235098"));
  ;; amount_t x2("$982340823.38");

  ;; {
  ;;   std::ostringstream bufstr;
  ;;   bufstr << x1;

  ;;   assert-value-equal(std::string("$982340823.386238098235098235098235098"), bufstr.str());
  ;; }

  ;; {
  ;;   std::ostringstream bufstr;
  ;;   bufstr << (x1 * x2).to_string();

  ;;   assert-value-equal(std::string("$964993493285024293.18099172508158508135413499124"), bufstr.str());
  ;; }

  ;; {
  ;;   std::ostringstream bufstr;
  ;;   bufstr << (x2 * x1).to_string();

  ;;   assert-value-equal(std::string("$964993493285024293.18"), bufstr.str());
  ;; }

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  )

(define-test serialization ()
  ;; amount_t x0;
  ;; amount_t x1("$8,192.34");
  ;; amount_t x2("8192.34");
  ;; amount_t x3("8192.34");
  ;; amount_t x4("-8192.34");
  ;; amount_t x5(x4);

  ;; // Force x3's pointer to actually be set to null_commodity
  ;; x3.set_commodity(*x3.current_pool->null_commodity);

  ;; std::string buf;
  ;; {
  ;;   std::ostringstream storage;
  ;;   assert-condition(x0.write(storage), amount_error);
  ;;   x1.write(storage);
  ;;   x2.write(storage);
  ;;   x3.write(storage);
  ;;   x4.write(storage);
  ;;   x5.write(storage);
  ;;   buf = storage.str();
  ;; }

  ;; amount_t x1b;
  ;; amount_t x2b;
  ;; amount_t x3b;
  ;; amount_t x4b;
  ;; amount_t x5b;
  ;; {
  ;;   std::istringstream storage(buf);
  ;;   x1b.read(storage);
  ;;   x2b.read(storage);
  ;;   x3b.read(storage);
  ;;   x4b.read(storage);
  ;;   x5b.read(storage);
  ;; }

  ;; assert-value-equal(x1, x1b);
  ;; assert-value-equal(x2, x2b);
  ;; assert-value-equal(x3, x3b);
  ;; assert-value-equal(x4, x4b);

  ;; const char * ptr = buf.c_str();

  ;; amount_t x1c;
  ;; amount_t x2c;
  ;; amount_t x3c;
  ;; amount_t x4c;
  ;; amount_t x5c;
  ;; {
  ;;   x1c.read(ptr);
  ;;   x2c.read(ptr);
  ;;   x3c.read(ptr);
  ;;   x4c.read(ptr);
  ;;   x5c.read(ptr);
  ;; }

  ;; assert-value-equal(x1, x1b);
  ;; assert-value-equal(x2, x2b);
  ;; assert-value-equal(x3, x3b);
  ;; assert-value-equal(x4, x4b);

  ;; assert-valid(x1);
  ;; assert-valid(x2);
  ;; assert-valid(x3);
  ;; assert-valid(x4);
  ;; assert-valid(x1b);
  ;; assert-valid(x2b);
  ;; assert-valid(x3b);
  ;; assert-valid(x4b);
  ;; assert-valid(x1c);
  ;; assert-valid(x2c);
  ;; assert-valid(x3c);
  ;; assert-valid(x4c);
  )

(textui-test-run (get-suite commodity-test-case))
(textui-test-run (get-suite amount-test-case))

(in-package :cl)
(ignore-errors
 (delete-package :cambl-test))

;; cambl-test.lisp ends here
