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

(defun solve (f min max epsilon)
  (let ((min-val (funcall f min))
        (max-val (funcall f max)))
    (cond ((< (abs min-val) epsilon) min)
          ((< (abs max-val) epsilon) max)
          ((> min-val max-val) (solve-recurse f max min epsilon))
          (t (solve-recurse f min max epsilon)))))
        
(defun solve-recurse (f min max epsilon)
  (let* ((mid (/ (+ min max) 2))
         (mid-val (funcall f mid)))
    (cond ((< (abs mid-val) epsilon) mid)
          ((> mid-val 0) (solve-recurse f min mid epsilon))
          (t (solve-recurse f mid max epsilon)))))
              