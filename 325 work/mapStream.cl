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

(defun map-stream (function stream)
  (do* ((not-interned (gensym))
        (current-line (read stream nil not-interned)
                      (read stream nil not-interned)))
       ((eql current-line not-interned) nil)
    (funcall function current-line)))

(defun map-file (f file)
  (with-open-file (str file :direction :input)
    (map-stream f str)))

(defun map-stream (fn stream)
  (let ((variable (gensym)))
    (do ((expression (read stream nil variable) (read stream nil variable))
         (output nil (funcall fn expression)))
        ((eql expression variable) nil))))



(defun map-file (fn name)
  (with-open-file (str name :direction :input)
    (map-stream fn str)))

(defun map-stream (fn in)
  (let ((obj (lambda (x) x)))
    (do ((expr (read in nil obj) (read in nil obj)))
      ((eql expr obj))
      (funcall fn expr))))