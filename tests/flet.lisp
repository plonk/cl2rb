(defun h (x)
  (flet ((f (n)
            (* x n))
         (g (n)
            (+ x n)))
    (print (f 2))
    (print (g 2))))

(h 3)
