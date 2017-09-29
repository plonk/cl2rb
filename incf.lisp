(let ( (x 10) )
  (print (incf x))
  (print x))
(terpri)

(let ( (x 10) )
  (print (decf x))
  (print x))
(terpri)

(let ( (x (list 10 10)) )
  (print (incf (car x)))
  (print x))
(terpri)

(let ( (x (list 10 10)) )
  (print (decf (car x)))
  (print x))
(terpri)

(defstruct hoge
  (a 10))

(let ( (x (make-hoge)) )
  (print (incf (hoge-a x)))
  (print (hoge-a x)))
(terpri)

(let ( (x (make-hoge)) )
  (print (decf (hoge-a x)))
  (print (hoge-a x)))
(terpri)


