(in-package :cs325-user)

(defparameter *facts* '(
  (bird isa animal)
  (dog isa animal)
  (pet isa animal)
  (tweety isa bird)
  (tweety isa pet)
  (rover isa dog)
  (rover isa pet)
  (animal can-fly no)
  (bird can-fly yes)
  ))

(defun query (triples &optional (blsts '(nil)))
  (cond ((null triples) blsts)
        ((null blsts) blsts)
        (t
         (query (cdr triples)
                (query-triple (car triples) blsts)))))

(defun query-triple (triple blsts)
  (mapcan (lambda (fact)
            (match-triple triple fact blsts))
          *facts*))

(defun match-triple (lst1 lst2 &optional (blsts '(nil)))
  (cond ((null blsts) nil)
        ((null lst1) blsts)
        (t
         (match-triple (cdr lst1) (cdr lst2)
             (match-item (car lst1) (car lst2) blsts)))))

(defun match-item (x y blsts)
  (cond ((eql x y) blsts)
        ((var-p x)
         (mapcan (lambda (blst) (match-var x y blst)) blsts))
        (t nil)))

(defun match-var (x y blst)
  (let ((bdg (assoc x blst)))
    (cond ((null bdg) (list (cons (list x y) blst)))
          ((equal y (cadr bdg)) (list blst))
          (t nil))))

(defun var-p (x)
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))