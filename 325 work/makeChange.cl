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

        



;; in the last critique you said:
;; collect counts only at the end using MAPCAR and COUNT
;; I'm a bit confused on this since I'm just returning the 
;; best list each time so I don't have a need.
;; I'm assuming that's to stop making new lists as often?
;; but then I must be holding the current answer in the wrong kind
;; of structure or something?
;; In any case this code seems clear and concise to me, but perhaps 
;; the efficiency of creating less lists somehow is better?

(defun make-best-change (num &optional (coins '(25 10 5 1)))
  (values-list (make-change num coins)))

(defun make-change (num coins)
  (cond ((= 0 num) (make-list (1+ (length coins)) :initial-element 0))
        ((null coins) (list num))
        ((<= (car coins) num) (get-better-coin-list
                               (cons 0 (make-change num (cdr coins)))
                               (let ((used-first-coin (make-change (- num (car coins)) coins)))
                                 (cons (1+ (car used-first-coin)) (cdr used-first-coin)))))
        (t (cons 0 (make-change num (cdr coins))))))

(defun get-better-coin-list (lst1 lst2)
  (cond ((> (car (last lst1)) (car (last lst2))) lst2)
        ((< (car (last lst1)) (car (last lst2))) lst1)
        ((> (reduce #'+ (butlast lst1)) (reduce #'+ (butlast lst2))) lst2)
        (t lst1)))


(defun make-best-change (target &optional (coins '(25 10 5 1)))
  (let ((best-coins (make-best-change-recurse target coins nil)))
    (values-list (mapcar (lambda (x) (count x best-coins)) coins))))

(defun make-best-change-recurse (target coins acc)
  (cond ((= 0 target) acc)
        ((null coins) acc)
        ((<= (car coins) target) 
         (get-better-coin-list (make-best-change-recurse 
                                (- target (car coins)) 
                                coins
                                (cons (car coins) acc))
                               (make-best-change-recurse 
                                target 
                                (cdr coins) 
                                acc)))
        (t (make-best-change-recurse target (cdr coins) acc))))

(defun get-better-coin-list (lst1 lst2)
  (let ((lst1-total (reduce #'+ lst1))
        (lst2-total (reduce #'+ lst2)))
    (cond ((> lst1-total lst2-total) lst1)
          ((> lst2-total lst1-total) lst2)
          ((> (length lst2) (length lst1)) lst1)
          (t lst2))))

                                  
  
  
  
  
  