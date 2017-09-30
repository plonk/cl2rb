(defun f ()
  999)

(defun g (x)
  (* 2 x))

(print (funcall #'f))
(print (funcall #'g 1024))