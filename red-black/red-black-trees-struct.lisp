

(in-package "RBT-TREES-STRUCT")



(declaim (inline init))





;(declaim (inline rotate-left))

;(declaim (inline rotate-right))



(defun rotate-left (x)

  (cond ((rbt-null x) nil)

        (t

         (let* ((y (right x))

                (b (left y)))

           (setf (right x) b)

           (setf (left y) x)

           (unless (rbt-null b)

             (setf (parent b) x))

           (unless (parent x)

             (setf (parent y) nil)

             (setf (parent x) y)

             (return-from rotate-left y))

           (setf (parent y) (parent x))

           (if (is-left-child x)

               (setf (left (parent x)) y)

             (setf (right (parent x)) y))

           (setf (parent x) y)

           nil))))



; the following code is dual under left-right x-y


(defun rotate-right (y)

  (cond ((rbt-null y) nil)

        (t

         (let* ((x (left y))

                (b (right x)))

           (setf (left y) b)

           (setf (right x) y)

           (unless (rbt-null b)

             (setf (parent b) y))

           (unless (parent y)

             (when (not (rbt-null x))
               
               (setf (parent x) nil))
               
             (setf (parent y) x)

             (return-from rotate-right x))
 
           (setf (parent x) (parent y))

           (if (is-right-child y)

               (setf (right (parent y)) x)

             (setf (left (parent y)) x))

           (setf (parent y) x)

           nil))))
   






;; back-values from insert item:

;; 1) new-root

;; 2) node-that-was-inserted-or-found

;; 3) item-already-been-in (t or nil)



(defun insert-item (it root &key (test-equal #'=) (test #'<) (key #'identity))

(let ((p) (n) (new-root root))

  (setf p (loop  with q = root

                 with p = nil
 
                 finally (return p)

                 while (not (rbt-null q))

                 do

                 (if (funcall test-equal (funcall key it) (funcall key (node-item q)))

                     (return-from insert-item (values root q t)))

                 (setf p q)

                 (if (funcall test (funcall key it) (funcall key (node-item q)))

                     (setf q (left q))

                   (setf q (right q)))))

  (setf n (init (make-rbt-node :node-item it :color :red :parent p)))

  (if p

      (if (funcall test (funcall key it) (funcall key (node-item p)))

          (setf (left p) n)

        (setf (right p) n))

    (setf new-root n))

  (setf new-root (fix-insert n new-root))

  (values new-root n nil)))


(declaim (inline left-to-grandparent)

         (inline right-to-grandparent)

         (inline uncle-over-right)

         (inline uncle-over-left)

         (inline is-right-child)

         (inline is-left-child)

         (inline blacken)

         (inline redden)

         (inline is-black)

         (inline is-red)

         (inline grand-parent))


(defun left-to-grandparent (node)

  (eql (parent node) (left (parent (parent node)))))


(defun right-to-grandparent (node)

  (eql (parent node) (right (parent (parent node)))))


(defun uncle-over-right (node)

  (right (parent (parent node))))


(defun uncle-over-left (node)

  (left (parent (parent node))))



(defun is-right-child (node)

  (eql node (right (parent node))))


(defun is-left-child (node)

  (eql node (left (parent node))))


(defun blacken (node)

  (setf (color node) :black))


(defun redden (node)

  (setf (color node) :red))



(defun is-red (node)

  (eql (color node) :red))


(defun is-black (node)

  (eql (color node) :black))
    

(defun grand-parent (node)

  (parent (parent node)))






(defun fix-insert (pivot root)

  (let ((new-root 

         (loop with y = nil

               with x = pivot

               with res = nil

               with new-root = root

               finally (return new-root)

               while (and (not (eql x new-root)) (is-red (parent x)))

               do

               (cond ((left-to-grandparent x)

                      (setf y (uncle-over-right x))

                      (cond ((is-red y)          

                             (blacken (parent x))

                             (blacken y)

                             (redden (grand-parent x))

                             (setf x (grand-parent x)))

                            (t
          

                             (when (is-right-child x)

                               (setf x (parent x))

                               (setf res (rotate-left x))

                               (when res

                                 (setf new-root res)))

                             (blacken (parent x))

                             (redden (grand-parent x))

                             (setf res (rotate-right (grand-parent x)))

                             (when res

                               (setf new-root res)))))

                     (t

                      (setf y (uncle-over-left x))

                      (cond ((is-red y)           

                             (blacken (parent x))

                             (blacken y)

                             (redden (grand-parent x))

                             (setf x (grand-parent x)))

                            (t
         
                             (when (is-left-child x)

                               (setf x (parent x))

                               (setf res (rotate-right x))

                               (when res

                                 (setf new-root res)))

                             (blacken (parent x))

                             (redden (grand-parent x))

                             (setf res (rotate-left (grand-parent x)))

                             (when res

                               (setf new-root res)))))))))

 

    (blacken new-root)

    new-root))



;
; find-item returns nil
; or
; pointer to the node in the tree root with item it
;

(defun find-item (it root &key (test-equal #'=) (test #'<) (key #'identity))

  (loop with p = root

     named find-loop

     finally (return-from find-loop nil)

     while (not (rbt-null p))

     do

     (cond ((funcall test-equal it (funcall key (node-item p)))
               
	    (return-from find-loop p))

	   ((funcall test it (funcall key (node-item p))) (setf p (left p)))

	   (t (setf p (right p))))))



;
;  delete-item returns
;  root t, if deletion done
;  root nil, if no deletion done
;



(defun delete-item (it root &key (test-equal #'=) (test #'<) (key #'identity))

  (let ((delnode (find-item it root :test-equal test-equal :test test :key key)))

    (if delnode

        (delete-node delnode root)

      (values root nil))))



;
;  delete-node returns
;  root t, if deletion done
;  root nil, if no deletion done
;


(defun delete-node (delnode root)

  (let ((y delnode)

        (x nil)

        (z nil)

        (newroot nil))


    (cond ((rbt-null y) (return-from delete-node (values root nil)))

          ((rbt-null (left y))

           (setf x delnode)

           (setf z (right y)))

          ((rbt-null (right y))

           (setf x delnode)

           (setf z (left y)))

          (t (loop with p = (right y)

                   while (not (rbt-null (left p)))

                   finally (setf x p)

                   do

                   (setf p (left p)))))

    (when (null z)   ;occurs iff case t in above cond selected
           
      (setf (node-item y) (node-item x))

      (setf z (right x)))

    (when (eql x root)

      (setf newroot z)

      (blacken z)

      (return-from delete-node (values newroot t)))

    (if (is-left-child x)

        (setf (left (parent x)) z)

      (setf (right (parent x)) z))

    (unless (rbt-null z)

      (setf (parent z) (parent x)))

    (when (is-black x)
            
      (when (is-red z)

        (blacken z)

        (return-from delete-node (values root t)))

      ; now (rbt-null z) is true

      (setf newroot (fix-delete z (parent x) root))
      
      (return-from delete-node (values newroot t)))

    (values root t)))



    

           

;
; this is my (JB) own version of the red-black correction after deletion
; it comprises 8 subcases and works upward from deletion-point towards root
; it returns the root of the rebalanced tree
;
;



(defun fix-delete (piv parpiv root)

  (let ((newpiv))

    (loop
     while (not (eql piv root))
     finally (return-from fix-delete root)
     do
     (setf newpiv t)

     (if (eql (left parpiv) piv)
         (let ((alpha parpiv)
               (beta)
               (gamma)
               (delta))

           (cond ((is-red alpha)
                  (setf beta (right alpha))
                  (setf gamma (left beta))
                  (cond ((is-black gamma)       ;1a
                         (setf root (or (rotate-left alpha) root)))
                        (t (setf root (or (rotate-right beta) root))  ; 1b
                           (setf root (or (rotate-left alpha) root))
                           (blacken alpha)))
                  (return-from fix-delete root))
                 ((is-black alpha)
                  (setf beta (right alpha))
                  (cond ((is-black beta)
                         (setf gamma (left beta))
                         (setf delta (right beta))
                         (cond ((is-red gamma)
                                (cond ((is-red delta)
                                       (redden beta)
                                       (blacken gamma)
                                       (blacken delta)) ; 2c -> 3
                                      (t (setf root (or (rotate-right beta) root))
                                         (setf root (or (rotate-left alpha) root))
                                         (blacken gamma)
                                         (return-from fix-delete root)))) ; 2b1
                               (t ; gamma is black, now decide if delta is black too (2a) or red (2b2)
                                (cond ((is-red delta)
                                       (setf root (or (rotate-left alpha) root))
                                       (blacken delta)
                                       (return-from fix-delete root)) ; this was 2b2
                                      (t ; now comes 2a
                                       (redden beta)
                                       (setf newpiv alpha))))))
                        (t ; this means beta is red, this gives cases 3a and 3b
                         (setf gamma (left beta))
                         (setf delta (left gamma))
                         (cond ((is-red delta)   ; this is 3b
                                (setf root (or (rotate-left alpha) root))
                                (setf root (or (rotate-right gamma) root))
                                (setf root (or (rotate-left alpha) root))
                                (blacken beta)
                                (return-from fix-delete root))
                               (t  ; this is 3a
                                (setf root (or (rotate-left alpha) root))
                                (setf root (or (rotate-left alpha) root))
                                (redden alpha)
                                (blacken beta)
                                (return-from fix-delete root))))))))

       ;the following code is dual under left-right
   
       (let ((alpha parpiv)
             (beta)
             (gamma)
             (delta))

         (cond ((is-red alpha)
                (setf beta (left alpha))
                (setf gamma (right beta))
                (cond ((is-black gamma)       ;1a
                       (setf root (or (rotate-right alpha) root)))
                      (t (setf root (or (rotate-left beta) root))  ; 1b
                         (setf root (or (rotate-right alpha) root))
                         (blacken alpha)))
                (return-from fix-delete root))
               ((is-black alpha)
                (setf beta (left alpha))
                (cond ((is-black beta)
                       (setf gamma (right beta))
                       (setf delta (left beta))
                       (cond ((is-red gamma)
                              (cond ((is-red delta)
                                     (redden beta)
                                     (blacken gamma)
                                     (blacken delta)) ; 2c -> 3
                                    (t (setf root (or (rotate-left beta) root))
                                       (setf root (or (rotate-right alpha) root))
                                       (blacken gamma)
                                       (return-from fix-delete root)))) ; 2b1
                             (t ; gamma is black, now decide if delta is black too (2a) or red (2b2)
                              (cond ((is-red delta)
                                     (setf root (or (rotate-right alpha) root))
                                     (blacken delta)
                                     (return-from fix-delete root)) ; this was 2b2
                                    (t ; now comes 2a
                                     (redden beta)
                                     (setf newpiv alpha))))))
                      (t ; this means beta is red, this gives cases 3a and 3b
                       (setf gamma (right beta))
                       (setf delta (right gamma))
                       (cond ((is-red delta)   ; this is 3b
                              (setf root (or (rotate-right alpha) root))
                              (setf root (or (rotate-left gamma) root))
                              (setf root (or (rotate-right alpha) root))
                              (blacken beta)
                              (return-from fix-delete root))
                             (t  ; this is 3a
                              (setf root (or (rotate-right alpha) root))
                              (setf root (or (rotate-right alpha) root))
                              (redden alpha)
                              (blacken beta)
                              (return-from fix-delete root)))))))))

                           

     (unless (eql newpiv t)

       (setf piv newpiv)

       (setf parpiv (parent piv))))))

                           


(defun nil-tree ()

  *sentinel*)


(defun select (root i)

  (cond ((rbt-null root) (values nil 0))

        (t (multiple-value-bind (leftwert leftanzahl) (select (left root) i)

             (if (null leftwert)

                 (cond ((= i leftanzahl)

                        (values (node-item root) nil))
                       
                       (t

                        (multiple-value-bind (rightwert rightanzahl) 

                            (select (right root) (- i leftanzahl 1))

                          (cond ((not (null rightwert))

                                 (values rightwert nil))

                                (t (values nil (+ leftanzahl rightanzahl 1)))))))

               (values leftwert nil))))))


  



;       
; the following are auxiliary functions used in testing the routines above
;


 
(defun tree-to-list (root &key (key #'identity))

  (cond

   ((rbt-null root) nil)

   (t (list 

       (format nil "Color: ~A - Item ~A" (color root) (funcall key (node-item root))) 

       (tree-to-list (left root)) 

       (tree-to-list (right root))))))


(defun make-item-list (root)

  (cond ((rbt-null root) nil)

        (t (let ((l (make-item-list (left root)))

                 (r (make-item-list (right root))))

             (append l (list (node-item root)) r)))))

                  



(defun make-test-tree (l)

  (let ((tree (nil-tree)))

    (loop for n in l

          finally (return tree)

          do

          (setf tree (insert-item n tree )))))



; check-rbt checks the rbt-conditions for the tree starting in root


(defun check-rbt (root)

  (labels ((check-recursive (node)

                            (cond ((rbt-null node) 1)

                                  (t (let ((a (check-recursive (left node)))

                                           (b (check-recursive (right node))))

                                       (if (or (null a) (null b) (/= a b))

                                           nil

                                         (cond ((is-black node) (1+ a))

                                               (t

                                                (if (and (is-black (left node))

                                                         (is-black (right node)))

                                                    a

                                                  nil)))))))))

    (and (is-black root) (check-recursive root))))
    
  

            

  

























