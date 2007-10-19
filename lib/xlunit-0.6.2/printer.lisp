;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:      $Id: printer.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;; Purpose: Printer functions for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)


;----------------------------------------------------------------------
; method print-results
;----------------------------------------------------------------------
 
(defmethod print-results ((obj textui-test-runner) result seconds)
  (print-header obj result seconds)
  (print-defects obj (errors result) "error")
  (print-defects obj (failures result) "failure")
  (print-footer obj result)
  (values))
 
(defmethod print-header ((obj textui-test-runner) result seconds)
  (declare (ignore result))
  (format (ostream obj) "~&Time: ~D~%~%" (coerce seconds 'float)))
                                                                                
(defmethod print-defects ((obj textui-test-runner) defects title)
  (when defects
    (let ((count (length defects)))
      (if (= 1 count)
	  (format (ostream obj) "~%There was 1 ~A:~%" title)
        (format (ostream obj) "~%There were ~D ~A:~%"
		count title))
      (dotimes (i count)
	(let* ((defect (nth i defects))
	       (condition (thrown-condition defect)))
	  (format (ostream obj) "~A) ~A: "
		  (1+ i) (name (failed-test defect)))
	  (typecase condition
	    (assertion-failed
	     (apply #'format (ostream obj) 
		    (simple-condition-format-control condition)
		    (simple-condition-format-arguments condition))
	     (format (ostream obj) "~%")
	     (when (message condition)
	       (let ((spaces (+ 2 (length (format nil "~D" count)))))
		 (dotimes (i spaces)
		   (write-char #\space (ostream obj))))
	       (format (ostream obj) "~A~%" (message condition))))
	    (t
	     (format (ostream obj) "~A~%" condition))))))))


(defmethod print-footer ((obj textui-test-runner) result)
  (let ((failures (failures result))
        (errors (errors result))
        (run-tests (run-tests result)))
    (cond ((and (null failures) (null errors))
           (format (ostream obj) "~%OK (~a tests)~%" run-tests))
          (t
           (format (ostream obj) "~%~%FAILURES!!!~%")
           (format (ostream obj) "Run: ~a   Failures: ~a   Errors: ~a~%"
                   run-tests (length failures) (length errors))))))

(defgeneric summary (result))
(defmethod summary ((result test-results))
  (format nil "~D run, ~D erred, ~D failed"
	  (run-tests result) (error-count result) (failure-count result)))
