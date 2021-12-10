(in-package :cs325-user)
(defparameter *frames*
  '(
    (dog (animal))
    (human (animal))
    (animal nil (can-fly no))
    (bird (animal) (can-fly yes))
    (penguin (bird) (can-fly no))
    (pet (animal) (owner human))
    (granny (human))
    (jon (human))
    (tweety (bird pet) (owner granny))
    (odie (dog pet) (owner jon))
    (chilly (penguin))
    ))

(defun linearize (x)
  (cons x (remove-duplicates (mapcan #'linearize (parents-of x)))))

(defun parents-of (x)
  (cadr (assoc x *frames*)))

(defparameter *all(lambda (frame)
            (linearize (car frame)))
          *frames*))
(defun get-frame (concept)
  (assoc concept *frames*))
(defun get-absts (concept)
  (assoc concept *all-absts*))



(defun get-filler (x y)
  (some (labda (a) (cadr (assoc y (cddr (get-frame a)))))
        (get-absts x)))