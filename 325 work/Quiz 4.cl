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

(define-test valid
  (assert-equal '((a a) (b b) (c c)) (valid '((a a) (b b) (c c))))
  (assert-false (valid '((a a) (b b) nil)))
  (assert-equal '((?x a) (b b) (c c)) (valid '((?x a) (b b) (c c))))
  (assert-equal '((?x a) (?y b) (?z c)) (valid '((?x a) (?y b) (?z c))))
  (assert-false (valid '((?x a) (b b) (?x c))))
  (assert-equal '((?x a) (b b) (?x a)) (valid '((?x a) (b b) (?x a))))
  )

(defun valid (bdgs)
  (and (every (lambda (bdg)
                (equal (assoc (car bdg) bdgs) bdg))
              bdgs)
       (every (lambda (bdg)
                (not (null bdg)))
                bdgs)
       bdgs))