(in-package :sddr-tests)

#|
Copyright (c) 2021 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; Test cases for the SDDR exercises

;;; UPDATES

;;; 2021/11/07 single name convention for Shakey exercises [CKR]
;;; 2021/11/05 added Shakey exercises [CKR]
;;; 2021/11/03 Revised DDR tests for SDDR  [CKR]
;;; 12/04/2013 Added Yuchen Yang's Shakey 2.0 test for robot in locked room [CKR]

;;; MEMBER
(defparameter *member-kb*
  '((<- (member ?x (cons ?x ?a)))
    (<- (member ?y (cons ?b ?c)) (member ?y ?c))))

(defparameter *map-color-kb*
  '(
    
    (<- (colors-for map1 ?a ?b ?c ?d)
        (color ?a)
        (color ?b)
        (color ?c)
        (color ?d)
        (different ?a ?b)
        (different ?a ?c)
        (different ?a ?d)
        (different ?b ?c)
        (different ?b ?d)
        (different ?c ?d))


    (<- (colors-for map2 ?a ?b ?c ?d ?e)
        (color ?a)
        (color ?b)
        (color ?c)
        (color ?d)
        (color ?e)
        (different ?a ?b)
        (different ?a ?c)
        (different ?a ?d)
        (different ?a ?e)
        (different ?b ?c)
        (different ?b ?e)
        (different ?c ?d)
        (different ?c ?e)
        (different ?d ?e))

    (<- (colors-for map3 ?a ?b ?c ?d ?e ?f)
        (color ?a)
        (color ?b)
        (color ?c)
        (color ?d)
        (color ?e)
        (color ?f)
        (different ?a ?b)
        (different ?a ?c)
        (different ?a ?d)
        (different ?a ?e)
        (different ?a ?f)
        (different ?b ?c)
        (different ?b ?d)
        (different ?b ?e)
        (different ?c ?d)
        (different ?d ?e)
        (different ?d ?f)
        (different ?e ?f))

    ))


(declaim (special *member-kb*))

(defun member-ask (query)
  (ask query *member-kb*))

(define-test member
  (assert-false (member-ask '(member a nil)))
  (assert-true (member-ask '(member a (cons a nil))))
  (assert-false (member-ask '(member nil (cons a nil))))
  (assert-true (member-ask '(member b (cons a (cons b (cons c nil))))))
  (assert-true (member-ask '(member c (cons a (cons b (cons c nil))))))
  (assert-false (member-ask '(member d (cons a (cons b (cons c nil))))))
  (assert-false (member-ask '(member nil nil)))
  (assert-false (member-ask '(member a a)))
  (assert-false (member-ask '(member a (cons (cons a nil) (cons b nil)))))
  )

;;; MAP COLORING

(declaim (special *map-color-kb*))

(defun map-ask (query)
  (ask query *map-color-kb*))

(define-test color-map1
  (assert-equal '((colors-for map1 red blue green yellow))
                (map-ask '(colors-for map1 red blue green ?d)))
  (assert-equal 2 (length (map-ask '(colors-for map1 red blue ?c ?d))))
  (assert-equal 24 (length (map-ask '(colors-for map1 ?a ?b ?c ?d))))
  (assert-equal nil (map-ask '(colors-for map1 red blue green red)))
    )

(define-test color-map2
  (assert-equal '((colors-for map2 red blue green blue yellow))
                (map-ask '(colors-for map2 red blue green ?d ?e)))
  (assert-equal 2 (length (map-ask '(colors-for map2 red blue ?c ?d ?e))))
  (assert-equal 24 (length (map-ask '(colors-for map2 ?a ?b ?c ?d ?e))))
  (assert-equal nil (map-ask '(colors-for map2 red blue green yellow ?e)))
  )

(define-test color-map3
  (assert-equal '((colors-for map3 red blue green yellow green blue))
                (map-ask '(colors-for map3 red blue green yellow ?e ?f)))
  (assert-equal 1 (length (map-ask '(colors-for map3 red blue green ?d ?e ?f))))
  (assert-equal 24 (length (map-ask '(colors-for map3 ?a ?b ?c ?d ?e ?f))))
  (assert-equal nil (map-ask '(colors-for map3 red blue green blue ?e ?f)))
  )


(define-test rename-vars
  (assert-true (valid-renaming-p '?x))
  (assert-true (valid-renaming-p '(?x ?x)))
  (assert-true (valid-renaming-p '(?x ?y)))
  (assert-true (valid-renaming-p '(foo ?x ?y (baz ?x ?y ?z))))
  (assert-equal nil (needless-cons-p '(foo b c)))
  (assert-equal nil (needless-cons-p '(foo ?x ?y (baz ?x ?y ?z))))
  (assert-equal nil (needless-cons-p '(foo (a (b c)) (d (e ?x)) (f g))))
  )

(defun valid-renaming-p (old &optional (new (sddr::rename-variables old)) (pairs '(nil)))
  (cond ((sddr::var-p old) (valid-pair-p old new pairs))
        ((atom old) (and (eql old new) pairs))
        (t 
         (valid-renaming-p (cdr old) (cdr new)
                           (valid-renaming-p (car old) (car new) pairs)))))

(defun valid-pair-p (old new pairs)
  (and (not (eql old new))
       (let ((pair (assoc old pairs)))
         (cond ((null pair) 
                (cons (cons old new) pairs))
               ((eql new (cdr pair)) pairs)
               (t nil)))))

;;; NOTE: CONTAINS-VAR-P is only for the tester. Do not use it
;;; in your RENAME-VARS. It would make your recursion very inefficient, 
;;; rescanning subtrees many times.

(defun needless-cons-p (old &optional (new (sddr::rename-variables old)))
  (and (consp old)
       (or (and (not (contains-var-p old))
                (not (contains-var-p new))
                (not (eq old new))
                old)
           (needless-cons-p (car old) (car new))
           (needless-cons-p (cdr old) (cdr new)))))

(defun contains-var-p (x)
  (or (sddr::var-p x)
      (and (consp x)
           (or (contains-var-p (car x))
               (contains-var-p (cdr x))))))
  
;;; Shakey Exercises

(declaim (special shakey-1 shakey-2 shakey-3))

;;; SHAKEY 1.0

;;; Test cases for 1 box, no locks.
;;;
;;; The goal state is always (v1-state ?rloc room1), meaning
;;; the box has to end up in room1, and it doesn't matter where
;;; the robot ends up.



(defparameter *plan-kb*
  '(
    (<- (plan nil ?goal ?goal))
   
    (<- (plan (cons ?action ?actions) ?current ?goal)
        (not (same ?current ?goal))
        (step ?action ?current ?goal)
        (results ?action ?current ?result)
        (plan ?actions ?result ?goal))
    
    (<- (same ?x ?x))
    ))

(defparameter shakey-1
  '(
    (<- (results (push-box ?bloc2)
                 (v1-state ?rloc1 ?rloc1)
                 (v1-state ?bloc2 ?bloc2)))
    
    (<- (results (move-to ?rloc2)
                 (v1-state ?rloc1 ?bloc)
                 (v1-state ?rloc2 ?bloc)))
    
    (<- (step (push-box hall)
              (v1-state ?bloc ?bloc)
              (v1-state ?rloc ?goal))
        (not (same ?bloc hall))
        (not (same ?goal ?bloc)))
    
    (<- (step (push-box ?goal)
              (v1-state hall hall)
              (v1-state ?any ?goal)))
    
    (<- (step (move-to ?bloc)
              (v1-state hall ?bloc)
              (v1-state ?any ?goal))
        (not (same hall ?bloc))
        (not (same ?bloc ?goal)))
    
    (<- (step (move-to hall)
              (v1-state ?rloc ?bloc)
              (v1-state ?any ?goal))
        (not (same ?rloc ?bloc))
        (not (same ?rloc hall))
        (not (same ?bloc ?goal)))           
    ))

(defun shakey-1 (query)
  (ask query (append *plan-kb* shakey-1))
  )

(define-test shakey-1
  (assert-equal
   '((plan nil (v1-state room1 room1) (v1-state room1 room1)))
   (shakey-1 '(plan ?actions
                        (v1-state room1 room1)
                        (v1-state room1 room1))))
  (assert-equal
   '((plan (cons (push-box room1) nil)
           (v1-state hall hall) 
           (v1-state room1 room1)))
   (shakey-1 '(plan ?actions
                        (v1-state hall hall) 
                        (v1-state room1 room1))))
  (assert-equal
   '((plan (cons (push-box hall)
                 (cons (push-box room1) nil))
           (v1-state room2 room2) 
           (v1-state room1 room1)))
   (shakey-1 '(plan ?actions
                        (v1-state room2 room2) 
                        (v1-state room1 room1))))
  (assert-equal
   '((plan (cons (move-to hall)
                 (cons (move-to room2)
                       (cons (push-box hall)
                             (cons (push-box room1) nil))))
           (v1-state room1 room2) 
           (v1-state room1 room1)))
   (shakey-1 '(plan ?actions
                        (v1-state room1 room2) 
                        (v1-state room1 room1))))
  )

;;; SHAKEY 2.0

;;; Test cases for 1 box with locks.
;;;
;;; The goal state is always
;;;
;;;   (v2-state ?rloc room1 ?unlocked)
;;;
;;; meaning the box has to end up in room1, and we don't care
;;; where the robot is or what rooms are unlocked.
(defparameter shakey-2
  '(
    (<- (member ?x (cons ?x ?a)))
    
    (<- (member ?y (cons ?b ?c)) (member ?y ?c))

    (<- (results (push-box ?bloc2)
                 (v2-state ?rloc1 ?rloc1 ?unlocked)
                 (v2-state ?bloc2 ?bloc2 ?unlocked)))
    
    (<- (results (move-to ?rloc2)
                 (v2-state ?rloc1 ?bloc ?unlocked)
                 (v2-state ?rloc2 ?bloc ?unlocked)))
    
    (<- (results (unlock ?room)
                 (v2-state ?rloc1 ?bloc ?unlocked)
                 (v2-state ?rloc1 ?bloc
                           (cons ?room ?unlocked))))
                
    (<- (step (unlock ?room)
              (v2-state hall ?room ?unlocked)
              (v2-state ?rloc ?goal ?diff-unlocked))
        (not (same ?room hall))
        (not (member ?room ?unlocked)))
    
    (<- (step (unlock ?room)
              (v2-state hall hall ?unlocked)
              (v2-state ?rloc ?room ?diff-unlocked))
        (not (member ?room ?unlocked)))
              
    (<- (step (push-box hall)
              (v2-state ?bloc ?bloc ?unlocked)
              (v2-state ?rloc ?goal ?diff-unlocked))
        (not (same ?bloc hall))
        (not (same ?goal ?bloc))
        (member ?bloc ?unlocked))
    
    (<- (step (push-box ?goal)
              (v2-state hall hall ?unlocked)
              (v2-state ?any ?goal ?diff-unlocked))
        (member ?goal ?unlocked))
    
    
    (<- (step (move-to ?bloc)
              (v2-state hall ?bloc ?unlocked)
              (v2-state ?any ?goal ?diff-unlocked))
        (member ?bloc ?unlocked)
        (not (same hall ?bloc))
        (not (same ?bloc ?goal)))
    
    (<- (step (move-to hall)
              (v2-state ?rloc ?bloc ?unlocked)
              (v2-state ?any ?goal ?diff-unlocked))
        (member ?rloc ?unlocked)
        (not (same ?rloc ?bloc))
        (not (same ?rloc hall))
        (not (same ?bloc ?goal)))           
    ))
(defun shakey-2 (query)
  (ask query (append *plan-kb* shakey-2))
  )


(define-test shakey-2  
  ;; Test with rooms unlocked
  (assert-equal
   '((plan nil
           (v2-state room1 room1 nil)
           (v2-state room1 room1 nil)))
   (shakey-2 '(plan ?actions
                        (v2-state room1 room1 nil)
                        (v2-state ?rloc room1 ?unlocked))))
  (assert-equal
   '((plan (cons (push-box room1) nil)
           (v2-state hall hall (cons room1 nil))
           (v2-state room1 room1 (cons room1 nil))))
   (shakey-2 '(plan ?actions
                        (v2-state hall hall (cons room1 nil))
                        (v2-state ?rloc room1 ?unlocked))))
  (assert-equal
   '((plan (cons (push-box hall) (cons (push-box room1) nil))
           (v2-state room2 room2 (cons room1 (cons room2 nil)))
           (v2-state room1 room1 (cons room1 (cons room2 nil)))))
   (shakey-2 '(plan ?actions
                        (v2-state room2 room2 (cons room1 (cons room2 nil)))
                        (v2-state ?rloc room1 ?unlocked))))
  (assert-equal
   '((plan (cons (move-to hall)
                 (cons (move-to room2)
                       (cons (push-box hall)
                             (cons (push-box room1) nil))))
           (v2-state room1 room2 (cons room1 (cons room2 nil)))
           (v2-state room1 room1 (cons room1 (cons room2 nil)))))
   (shakey-2 '(plan ?actions
                        (v2-state room1 room2 (cons room1 (cons room2 nil)))
                        (v2-state ?rloc room1 ?unlocked))))
  
  ;; Test with the room with the box locked
  (assert-equal
   '((plan (cons (move-to hall)
                 (cons (unlock room2)
                       (cons (move-to room2)
                             (cons (push-box hall)
                                   (cons (push-box room1) nil)))))
           (v2-state room1 room2 (cons room1 nil))
           (v2-state room1 room1 (cons room2 (cons room1 nil)))))
   (shakey-2 '(plan ?actions
                        (v2-state room1 room2 (cons room1 nil))
                        (v2-state ?rloc room1 ?unlocked))))
  
  ;; Test with the goal room locked, robot and box in hall
  (assert-equal
   '((plan (cons (unlock room1)
                 (cons (push-box room1) nil))
           (v2-state hall hall nil)
           (v2-state room1 room1 (cons room1 nil))))
   (shakey-2 '(plan ?actions
                        (v2-state hall hall nil)
                        (v2-state ?rloc room1 ?unlocked))))

  ;; Test with the goal room locked, robot in hall, box in room2
  (assert-equal
   '((plan (cons (move-to room2)
                 (cons (push-box hall)
                       (cons (unlock room1)
                             (cons (push-box room1) nil))))
           (v2-state hall room2 (cons room2 nil))
           (v2-state room1 room1 (cons room1 (cons room2 nil)))))
   (shakey-2 '(plan ?actions
                        (v2-state hall room2 (cons room2 nil))
                        (v2-state ?rloc room1 ?unlocked))))

  ;; Test with both rooms locked and the robot in the hall
  (assert-equal
   '((plan (cons (unlock room2)
                 (cons (move-to room2)
                       (cons (push-box hall)
                             (cons (unlock room1)
                                   (cons (push-box room1) nil)))))
           (v2-state hall room2 nil)
           (v2-state room1 room1 (cons room1 (cons room2 nil)))))
   (shakey-2 '(plan ?actions
                        (v2-state hall room2 nil)
                        (v2-state ?rloc room1 ?unlocked))))
  
  ;; Test with robot in locked room, box in another
  (assert-equal
   nil
   (shakey-2 '(plan ?actions
                        (v2-state room1 room2 nil)
                        (v2-state ?rloc room1 ?unlocked))))
  
  ;; Test with robot in locked room with box (from Yuchen Yang)
  (assert-equal
   nil
   (shakey-2 '(plan ?actions
                        (v2-state room1 room1 nil)
                        (v2-state ?rloc room2 ?unlocked))))
  )

;;; SHAKEY 3.0

;;; Test cases for N boxes with locks, going to the same room.
;;;
;;; v3-state takes an initial robot location, a list of boxes
;;; to collect, a room to move the boxes to, and a list of
;;; unlocked rooms.
;;;
;;; The goal state is no more boxes to move:
;;;
;;;   (v3-state ?rloc nil ?gloc ?unlocked)
(defparameter *plan-kb*
  '(
    (<- (plan nil ?goal ?goal))
   
    (<- (plan (cons ?action ?actions) ?current ?goal)
        (not (same ?current ?goal))
        (step ?action ?current ?goal)
        (results ?action ?current ?result)
        (plan ?actions ?result ?goal))
    
    (<- (same ?x ?x))
    ))
;;
;; (<- (plan ?total-plan 
;;              (v3-state ?current-rl ?current-brl ?goal-room ?current-unlocked)
;;              (v3-state ?end-rl nil ?goal-room ?end-unlocked))
;;        (not (same ?current-brl nil))
;;        (plan ?plan1 
;;              (v2-state ?current-rl ?b1-loc ?current-unlocked)
;;              (v2-state ?any-rl ?goal-room ?diff-unlocked))
;;        (member ?b1-loc ?current-brl)
;;        (append (cons ?b1-loc nil) ?shortened-brl ?current-brl)
;;        (append ?plan1 ?plan2 ?total-plan) 
;;        (plan ?plan2
;;              (v3-state ?any-rl ?shortened-brl ?goal-room ?diff-unlocked)
;;              (v3-state ?end-rl nil ?goal-room ?end-unlocked))
        
        
 ;;       )
(defparameter shakey-3
  '(
    
    (<- (plan ?total-plan
              (v3-state ?current-rl (cons ?first ?rest) ?goal-room ?current-unlocked)
              (v3-state ?end-rl nil ?goal-room ?end-unlocked))
        (plan ?plan1
              (v2-state ?current-rl ?first ?current-unlocked)
              (v2-state ?any-rl ?goal-room ?diff-unlocked))
        (plan ?plan2
              (v3-state ?any-rl ?rest ?goal-room ?diff-unlocked)
              (v3-state ?end-rl nil ?goal-room ?end-unlocked))
        (append ?plan1 ?plan2 ?total-plan))

    (<- (append nil ?x ?x))
    (<- (append (cons ?x ?l1) ?l2 (cons ?x ?l3))
        (append ?l1 ?l2 ?l3))
    
    (<- (member ?x (cons ?x ?a)))
    
    (<- (member ?y (cons ?b ?c)) (member ?y ?c))
    
    (<- (results (push-box ?bloc2)
                 (v2-state ?rloc1 ?rloc1 ?unlocked)
                 (v2-state ?bloc2 ?bloc2 ?unlocked)))
    
    (<- (results (move-to ?rloc2)
                 (v2-state ?rloc1 ?bloc ?unlocked)
                 (v2-state ?rloc2 ?bloc ?unlocked)))
    
    (<- (results (unlock ?room)
                 (v2-state ?rloc1 ?bloc ?unlocked)
                 (v2-state ?rloc1 ?bloc
                           (cons ?room ?unlocked))))
    
    (<- (step (unlock ?room)
              (v2-state hall ?room ?unlocked)
              (v2-state ?rloc ?goal ?diff-unlocked))
        (not (same ?room hall))
        (not (member ?room ?unlocked)))
    
    (<- (step (unlock ?room)
              (v2-state hall hall ?unlocked)
              (v2-state ?rloc ?room ?diff-unlocked))
        (not (member ?room ?unlocked)))
    
    (<- (step (push-box hall)
              (v2-state ?bloc ?bloc ?unlocked)
              (v2-state ?rloc ?goal ?diff-unlocked))
        (not (same ?bloc hall))
        (not (same ?goal ?bloc))
        (member ?bloc ?unlocked))
    
    (<- (step (push-box ?goal)
              (v2-state hall hall ?unlocked)
              (v2-state ?any ?goal ?diff-unlocked))
        (member ?goal ?unlocked))
    
    
    (<- (step (move-to ?bloc)
              (v2-state hall ?bloc ?unlocked)
              (v2-state ?any ?goal ?diff-unlocked))
        (member ?bloc ?unlocked)
        (not (same hall ?bloc))
        (not (same ?bloc ?goal)))
    
    (<- (step (move-to hall)
              (v2-state ?rloc ?bloc ?unlocked)
              (v2-state ?any ?goal ?diff-unlocked))
        (member ?rloc ?unlocked)
        (not (same ?rloc ?bloc))
        (not (same ?rloc hall))
        (not (same ?bloc ?goal)))           
    ))


(defun shakey-3 (query)
  (ask query (append *plan-kb* shakey-3))
  )

(define-test shakey-3
  ;; Test already done case
  (assert-equal
   '((plan
      nil
      (v3-state room1 nil room2 nil)
      (v3-state room1 nil room2 nil)))
   (shakey-3
    '(plan ?actions
           (v3-state room1 nil room2 nil)
           (v3-state ?rloc nil ?gloc ?unlocked))))
  
  ;; Test with 1 box, all rooms locked, the robot in the hall
  (assert-equal
   '((plan (cons (unlock room2)
                 (cons (move-to room2)
                       (cons (push-box hall)
                             (cons (unlock room1)
                                   (cons (push-box room1) nil)))))
           (v3-state hall (cons room2 nil) room1 nil)
           (v3-state room1 nil room1 (cons room1 (cons room2 nil)))))
   (shakey-3
    '(plan ?actions
           (v3-state hall (cons room2 nil) room1 nil)
           (v3-state ?rloc nil room1 ?unlocked))))
  
  ;; Test with 2 boxes, all rooms locked, the robot in the hall
  (assert-equal
   '((plan
      (cons (unlock room2)
            (cons (move-to room2)
                  (cons (push-box hall)
                        (cons (unlock room1)
                              (cons (push-box room1) 
                                    (cons (move-to hall)
                                          (cons (unlock room3)
                                                (cons (move-to room3)
                                                      (cons (push-box hall)
                                                            (cons (push-box room1)
                                                                  nil))))))))))
      (v3-state hall (cons room2 (cons room3 nil)) room1 nil)
      (v3-state room1 nil room1 (cons room3 (cons room1 (cons room2 nil))))))
   (shakey-3
    '(plan ?actions
           (v3-state hall (cons room2 (cons room3 nil)) room1 nil)
           (v3-state ?rloc nil room1 ?unlocked))))
  )
