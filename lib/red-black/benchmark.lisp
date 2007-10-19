;
; super-test creates n trees, inserts k random items and deletes them in random order
; checking the correctness of the tree after every deletion  
;


(defparameter *test-list-array* nil)



(defun make-items-hash (len)

  (let ((ht (make-hash-table  :size (* 2 len)))
        (rndlim 100000000))

    (do ((cnt 0)
         (rnd (random rndlim) (random rndlim)))
        ((= cnt len) ht)
      (if (not (gethash rnd ht nil))
          (let ()
            (setf (gethash rnd ht) cnt)
            (setf cnt (1+ cnt)))))))


(defun super-test (n k)

  (let ()

    (dotimes (i n)

      (let* ((len k)
             (extra-time 0)
             (items-hash (make-items-hash k))
             (root (rbt:nil-tree)))

        (if (zerop (mod i 50)) (format t "i = ~A~%" i))

        (maphash #'(lambda (key v) (setf root (rbt:insert-item key root))) items-hash)

        (if (null (rbt:check-rbt root))
            (error "RBT-property not fulfilled"))

;       (setf lis (loop for j from 0 to (- k 2) collect (select root j)))
;       (setf lis (butlast lis 5))

        (maphash #'(lambda (key v) (setf root (rbt:delete-item key root))) items-hash)
            
        (when (or (null (rbt:check-rbt root)) (not (rbt:rbt-null root)))

          (setf err-tree root)
          (error "Mistake in delete-function"))))))

;      (gapp:graph-app (tree-list root))

;      (format nil "Len lis = ~A ~%" (length (item-list root))))))
