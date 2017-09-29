(print (let ((x '(1))) (push 2 x) x)) ; (2 1)
(print (let ((x '())) (push 2 x) x))  ; (2)
(print (let ((x ())) (push 2 x) x))   ; (2)
(print (let ((x nil)) (push 2 x) x))  ; (2)

