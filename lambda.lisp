(let ((f (lambda () 100)))
  (print (funcall f)))

(let ((f (lambda (x) (* x 111))))
  (print (funcall f 9)))

(print (funcall (lambda (x) (* x 111)) 8))