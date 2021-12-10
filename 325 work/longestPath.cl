(defun longest-path (start goal graph)
  (or (reverse (dfs-long goal (list start) graph)) 
      (and (eql start goal) (list start))))
      
(defun dfs-long (goal current graph)
  (do ((neighbors (cdr (assoc (car current) graph)) (cdr neighbors))
       (max nil (path> max (find-path (car neighbors) current graph goal))))
      ((null neighbors) max)))

(defun find-path (neighbor current graph goal)
  (cond ((null neighbor) nil)
        ((eql goal neighbor) 
         (cons neighbor current))
        ((member neighbor current) nil)
        (t (dfs-long goal (cons neighbor
                                current) graph))))

(defun path> (some-path other-path)
  (cond ((> (length some-path) (length other-path)) some-path)
        (t other-path)))

