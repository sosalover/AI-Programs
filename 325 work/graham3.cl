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


(defun has-number-p (s-exp)
  (cond ((atom s-exp)
         (numberp s-exp))
        (t (some #'has-number-p s-exp))))

(defun show-dots (lst)
  (cond
   ((atom lst) (format t "~s" lst))
   (t (format t "(")
      (show-dots (car lst))
      (format t " . ")
      (show-dots (cdr lst))
      (format t ")"))))

(defun show-list (lst)
  (cond 
   ((atom lst) (format t "~s" lst))
   (t (format t "[") 
      (show-list (car lst)) 
      (do ((rest-of-lst (cdr lst) (cdr rest-of-lst)))
          ((atom rest-of-lst) (cond 
                               ((null rest-of-lst) nil)
                               (t (format t " . ~s" rest-of-lst))))
        (format t " ") 
        (show-list (car rest-of-lst)))
      (format t "]"))))

