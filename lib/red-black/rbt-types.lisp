
(in-package "RBT-TREES-STRUCT")



(defstruct (bnt-node (:conc-name nil)

                     (:print-function (lambda (nd s dep) (format s "Node <~A>" (node-item nd)))))

  (node-item nil)

  (left nil)

  (right nil))






(defstruct (rbt-node (:include bnt-node) (:conc-name nil))

  (color :black)
   
  (parent nil))




(defparameter *sentinel* (make-rbt-node))



;(setf *old-print-level* *print-level*)
;(setf *print-level* 1)



(setf (parent *sentinel*) nil)

(setf (left *sentinel*) *sentinel*)

(setf (right *sentinel*) *sentinel*)


;(setf *print-level* *old-print-level*)



(declaim (inline rbt-null))


(defun rbt-null (x)

  (eql *sentinel* x))


(declaim (inline init))



(defun init (node)

  (setf (left node) *sentinel*)

  (setf (right node) *sentinel*)

  node)







