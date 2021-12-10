(ql:quickload "mops")
(in-package :mop-tests)
(load-kb *mop-file*)

(run-tests has-slots-p)



(defun has-slots-p (mop slots)
  (notany #'(lambda (slot) 
              (multiple-value-bind (filler contained) 
                  (get-filler mop (car slot)) 
                (not (and contained (isa-p filler (cadr slot)))))) 
          slots))

