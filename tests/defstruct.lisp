(defstruct hoge
  (x 0)
  (y 1))

(print (hoge-x (make-hoge)))
(print (hoge-y (make-hoge)))

(let ((h (make-hoge :x 55 :y 99)))
  (print (hoge-x (make-hoge :x 55 :y 99)))
  (print (hoge-y (make-hoge :x 55 :y 99))))

(defstruct fuga
  (long-long-name t))

(print (fuga-long-long-name (make-fuga)))

(let ((h (make-hoge)))
  (print (hoge-x h))
  (setf (hoge-x h) 10000)
  (print (hoge-x h)))

(print (hoge-p (make-hoge)))
(print (hoge-p (make-fuga)))
(print (hoge-p t))
(print (hoge-p nil))
(print (hoge-p 1))
