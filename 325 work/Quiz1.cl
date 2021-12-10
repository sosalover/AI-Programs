(in-package : cs325-user)

(defparameter *facts*
  '((bird isa animal)
    (pet isa animal)
    (canary isa bird)
    (penguin isa bird)
    (animal can-fly no)
    (bird can-fly yes)
    (penguin can-fly no)
    (tweety isa pet)
    (tweety isa canary)
    (chilly isa penguin)
    ))

(defun foo (x)
  (cons x 
        (mapcan (lambda (fact)
                  (and (eql x (car fact))
                       (eql 'isa (cadr fact))
                       (foo (caddr fact))))
                *facts*)))

(defun baz (x)
  (remove-duplicates (foo x)))

(defun show-results (fn)
  (dolist (x '(animal bird canary tweety penguin chilly))
    (format t "~%~S => ~S" x (funcall fn x))))

(defun lookup (x y)
  (find-if (lambda (fact)
             (and (eql (car fact) x) (eql (cadr fact) y)))
           *facts*))

(defun get-prop (x y)
  (some (lambda (abst) (lookup abst y))
        (baz x)))

(defun show-props (prop)
  (let ((concepts (remove-duplicates (mapcar 'car *facts*))))
    (dolist (x concepts)
      (format t "~%~S => ~S" x (get-prop x prop)))))
