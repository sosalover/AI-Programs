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

(defun mt (lst1 lst2)
  (valid (mapcar 'mi lst1 lst2)))

(defun mi (x y)
  (and (or (eql x y) (var-p x)) (list x y)))

(defun valid (bdgs)
  (and (every (lambda (bdg) (eql (assoc (car bdg) bdgs) bdgs))bdgs)                             
                bdgs)
       bdgs)

;;; don't worry about how this works
(defun var-p (x)
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun show-mt (lst1 lst2)
  (format t "~%~S ~S => ~S" lst1 lst2 (mt lst1 lst2)))

(defun test-mt ()
  (show-mt '(a b c) '(a b c))
  (show-mt '(a b c) '(a b b))
  (show-mt '(?x b c) '(a b c))
  (show-mt '(?x ?y ?z) '(a b c))
  (show-mt '(?x b ?x) '(a b c))
  )