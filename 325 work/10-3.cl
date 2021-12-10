

(defmacro nth-expr (n &body body)
  (let ((gn (gensym)))
    `(let ((,gn (1- ,n)))
       (case ,gn
         ,@(let ((i -1))
             (mapcar #'(lambda (x) `(,(incf i) ,x)) body))))))

(defmacro n-of (n expr)
  (let ((gn (gensym))
        (gi (gensym))
        (glst (gensym)))
    `(do ((,gn ,n) (,gi 0 (1+ ,gi)) (,glst nil (cons ,expr ,glst)))
         ((= ,gi ,gn) (nreverse ,glst)))))