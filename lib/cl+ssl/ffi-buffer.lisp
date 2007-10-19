(in-package :cl+ssl)

(defun make-buffer (size)
  (cffi-sys::make-shareable-byte-vector size))

(defun buffer-length (buf)
  (length buf))

(defun buffer-elt (buf index)
  (elt buf index))
(defun set-buffer-elt (buf index val)
  (setf (elt buf index) val))
(defsetf buffer-elt set-buffer-elt)

(defun v/b-replace (vec buf &key (start1 0) end1 (start2 0) end2)
  (replace vec buf :start1 start1 :end1 end1 :start2 start2 :end2 end2))
(defun b/v-replace (buf vec &key (start1 0) end1 (start2 0) end2)
  (replace buf vec :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defmacro with-pointer-to-vector-data ((ptr buf) &body body)
  `(cffi-sys::with-pointer-to-vector-data (,ptr ,buf)
    ,@body))
