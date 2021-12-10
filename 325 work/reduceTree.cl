
       
(defun reduce-tree (f tree &optional (initial-value nil))
  (cond ((null tree) initial-value)
        ((atom tree) (funcall f initial-value tree))
        (t (reduce-tree f (cdr tree) (reduce-tree f (car tree) initial-value)))))