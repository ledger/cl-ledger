(in-package :cl-user)

(defpackage :rbt-trees-struct-asd
  (:use :cl :asdf))

(in-package :rbt-trees-struct-asd)

(defvar *rbt-trees-struct-version* "1.0"
  "A string denoting the current version of this red-black tree library.  Used
for diagnostic output.")

(export '*rbt-trees-struct-version*)

(asdf:defsystem :rbt-trees-struct
  :serial t
  :version #.*rbt-trees-struct-version*
  :components ((:file "rbt-trees-package")
	       (:file "rbt-types")
	       (:file "red-black-trees-struct")))
