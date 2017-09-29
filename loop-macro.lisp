(loop for i from 1 to 10
      do (print i))

(loop for i from 1 below 10
      do (print i))

(loop for k in '(a b c)
      for i from 1 below 2
      do
      (print i)
      (print k))