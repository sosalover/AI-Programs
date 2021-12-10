; You previously critiqued "Why bind the variable to one value only to immediately re-assign another value?"
; I'm not sure if you were referring to an unecessary remove when returning result, or if you meant the setq statement.
; The setq statement I include because I don't see a way to change result to capital or lowercase without using nested,
; repeated cond's, and I find this set to be more concise. If there's another way, please give me some direction.

(defun camelize (str &optional (capitalize nil))
  (let ((location (position #\- str)))
    (let ((result (cond (location
                         (delete  #\- (string-capitalize str :start location)))
                        (t str))))
      (cond (capitalize (string-capitalize result :start 0 :end 1))
            (t result)))))
                                   
; You previously critiqued "Why build your own array when with-output-to-string will do it for you?" When I tried to 
; inintialize result to (with-output-to-string (s)) it said my array was missing a fill pointer, and I found some
; documentation using make-array with with-output-to-string which solved the issue. If this is inefficient, and there's
; another way, please give me some direction.

  
(defun hyphenate (str &optional (case :upper))
  (let ((fmt (ecase case
               (:lower "~(~c~)")
               (:upper "~:@(~c~)"))))
    (with-output-to-string (s)
      (format s fmt (char str 0))
      (do ((i 2 (1+ i))
            (previous (char str 0) current)
            (current (char str 1) (char str i)))
           ((eql i (length str))
            (format-conditional s fmt current previous))
        (format-conditional s fmt current previous)))))
  
(defun format-conditional (s fmt current previous)
  (cond ((and (upper-case-p current) (lower-case-p previous))
         (format s fmt #\-)
         (format s fmt current))
        (t (format s fmt current))))
                           
  
  
                             

;;; CAMELIZE

(define-test camelize
  (assert-equal "job" (camelize "job"))
  (assert-equal "Job" (camelize "job" t))
  (assert-equal "jobPosting" (camelize "job-posting"))
  (assert-equal "BookFormatType" (camelize "book-format-type" t))
  (assert-equal "JobPosting" (camelize "job-posting" t))
)

;;; HYPHENATE

(define-test hyphenate
  (assert-equal "JOB" (hyphenate "job"))
  (assert-equal "JOB" (hyphenate "Job"))
  (assert-equal "JOB" (hyphenate "Job" :upper))
  (assert-equal "job" (hyphenate "Job" :lower))
  (assert-equal "JOB-POSTING" (hyphenate "jobPosting"))
  (assert-equal "JOB-POSTING" (hyphenate "JobPosting"))
  (assert-equal "BOOK-FORMAT-TYPE" (hyphenate "BookFormatType"))
  (assert-equal "URL" (hyphenate "URL"))
  (assert-equal "GET-ID" (hyphenate "getID"))
  (assert-equal "AN-A" (hyphenate "anA"))
  )