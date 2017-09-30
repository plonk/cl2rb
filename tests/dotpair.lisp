(print (car '(1 2)))
(print (cdr '(1 2)))

(print (car '(1 . 2)))
(print (cdr '(1 . 2)))

(let ((ls (list 1 2)))
  (setf (car ls) 3)
  (setf (cdr ls) '(4))
  (print ls))

(let ((ls '(1 . 2)))
  (setf (car ls) 3)
  (setf (cdr ls) 4)
  (print ls))

(exit)
