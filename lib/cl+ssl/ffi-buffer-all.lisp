(in-package :cl+ssl)

(defconstant +initial-buffer-size+ 2048)

(declaim
 (inline
  make-buffer
  buffer-length
  buffer-elt
  set-buffer-elt
  v/b-replace
  b/v-replace))
