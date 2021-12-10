;;; This is the IDE's built-in-editor, where you create and edit
;;; lisp source code.  You could use some other editor instead,
;;; though the IDE's menu-bar commands would not be applicable there.
;;; 
;;; This editor has a tab for each file that it's editing.  You can
;;; create a new editor buffer at any time with the File | New command.
;;; Other commands such as Search | Find Definitions will create
;;; editor buffers automatically for existing code.
;;; 
;;; You can use the File | Compile and Load command to compile and
;;; load an entire file, or compile an individual definition by
;;; placing the text cursor inside it and using Tools | Incremental
;;; Compile.  You can similarly evaluate test expressions in the
;;; editor by using Tools | Incremental Evaluation; the returned
;;; values and any printed output will appear in a lisp listener
;;; in the Debug Window.
;;; 
;;; For a brief introduction to other IDE tools, try the
;;; Help | Interactive IDE Intro command.  And be sure to explore
;;; the other facilities on the Help menu.

(in-package :cs325-user)

(defun has-list-p (lst)
  (cond ((null lst) nil)
        ( (listp (car lst)) t)
        ( t (has-list-p (cdr lst)))
        )
  )

(defun greater (x y)
  (cond ( (> x y) x)
        ( t y))
  )

(defun print-dots (x)
  (do ((i 0 (1+ i)))
      ((>= i x))
    (format t "."))
  )

(defun print-dots (x)
  (cond ( (<= x 0) nil)
        ( t (format t ".") (print-dots (1- x))
           ))
  )

(defun get-a-count (lst)
  (cond ((null lst) 0)
        ((eql 'A (car lst)) (1+ (get-a-count (cdr lst))))
        (t (get-a-count (cdr lst)))
        )
  )

(defun get-a-count (lst)
  (do ((x lst (cdr x)) 
       (count 0
              (cond ((eql (car x) 'a) (1+ count))
                    (t count)
                    )
              )
       )
      ((null x) count)
    )
  )

(defun summit (lst) 
  (cond ((null lst) 0)
        (t (let ((x (car lst))) 
             (cond ((null x) (summit (cdr lst))) 
                   (t(+ x (summit (cdr lst)))))))))

(defun summit (lst) 
  (apply #'+ (remove nil lst)) 
  )
