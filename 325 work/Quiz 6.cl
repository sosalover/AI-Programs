(in-package :cs325-user)

(defparameter *facts*
  '(
    (bird isa animal)
    (dog isa animal)
    (pet isa animal)
    (human isa animal)
    (animal can-fly no)
    (bird can-fly yes)
    (pet owner human)
    (granny isa human)
    (jon isa human)
    (tweety isa bird)
    (tweety isa pet)
    (tweety owner granny)
    (odie isa dog)
    (odie isa pet)
    (odie owner jon)
    ))

(defun make-frames ()
  (mapcar (lambda (x)
            (cons x (cons (make-isas x) (make-props x))))
          (make-concepts)))


(defun make-concepts ()
    *FACTS*
  )
(defun make-isas (x)
  (mapcar (lambda (y) (cond ((and (equal (car y) x) 
                                  (equal (cadr y) 'isa)) 
                             (caddr y))
                            (t nil))
            )
    *FACTS*))

(defun make-props (x)
  (mapcar (lambda (y) (cond ((and (equal (car y) x) 
                                  ((not (equal (cadr y) 'isa))))
                                  '((cadr y) (caddr y)))
                                  (t nil))
                             )
                            *FACTS*))

;;; desired output of make-frames
(defparameter *frames*
  '(
    (DOG (ANIMAL))
    (HUMAN (ANIMAL))
    (ANIMAL NIL (CAN-FLY NO))
    (BIRD (ANIMAL) (CAN-FLY YES))
    (PET (ANIMAL) (OWNER HUMAN))
    (GRANNY (HUMAN))
    (JON (HUMAN))
    (TWEETY (BIRD PET) (OWNER GRANNY))
    (ODIE (DOG PET) (OWNER JON))
    ))