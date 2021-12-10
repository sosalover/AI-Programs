

(defun max-min (vec &key (start 0) (end (length vec)))
  (cond ((> (1+ start) end)
         (values nil nil))
        (t (max-min-recurse vec 
                            (1+ start) 
                            end 
                            (aref vec start) 
                            (aref vec start)))))

(defun max-min-recurse (vec start end current-min current-max)
  (cond ((= start end) (values current-min current-max))
        (t 
         (let ((element1 (aref vec start)))
           (multiple-value-bind (current-max current-min) 
               (max-min-recurse vec (1+ start) end current-min current-max)
             (values (max element1 current-max) (min element1 current-min)))))))
