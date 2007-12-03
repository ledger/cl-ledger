;; sort.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun sort-entries (xact-series &key (test #'value<) (key #'xact-amount))
  (scan-lists-of-lists-fringe
   (mapcar #'(lambda (entry-xacts)
	       (sort entry-xacts test :key key))
	   ;; `reduce' is used here to "clump" the incoming stream of
	   ;; transactions into sublists where adjacent transactions having
	   ;; the same parent entry become part of the same sublist.
	   ;;
	   ;; Consider this input stream, where the first letter identifies
	   ;; the entry, and the second identifies the membeer transaction:
	   ;;
	   ;;   (A-X A-Y A-Z B-X B-Y C-X)
	   ;;
	   ;; Given this input, the resulting list from `reduce' will be:
	   ;;
	   ;;   ((A-X A-Y A-Z) (B-X B-Y) (C-X))
	   ;;
	   ;; By segegrating adjacent transactions into sublists, we can
	   ;; easily sort those sublists and then use
	   ;; `scan-lists-of-lists-fringe' to produce a series that walks the
	   ;; individual transactions again.
	   (nreverse
	    (reduce #'(lambda (entries xact)
			(if entries
			    (if (eq (xact-entry (caar entries))
				    (xact-entry xact))
				(cons (cons xact (first entries))
				      (rest entries))
				(cons (list xact) entries))
			    (list (list xact))))
		    (collect xact-series)
		    :initial-value '())))))

(defun sort-transactions (xact-series &key (test #'value<) (key #'xact-amount))
  (scan (sort (collect xact-series) test :key key)))

(provide 'sort)

;; sort.lisp ends here
