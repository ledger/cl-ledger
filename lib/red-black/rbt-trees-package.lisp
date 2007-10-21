

(defpackage "RBT-TREES-STRUCT"

  (:use "COMMON-LISP")
  (:nicknames "RBT" "RBT-TREES")
  

  (:export

   nil-tree

   rbt-node

   rbt-null

   node-item

   right

   left

   insert-item

   find-item

   delete-item

   delete-node

   select

   tree-to-list

   make-items-hash

   make-test-tree

   check-rbt

   prepare-test-lists

   super-test))


