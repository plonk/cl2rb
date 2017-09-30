(defstruct a->a
 (x 10))

(defstruct (b->b (:include a->a))
  (y 20))

(let ((b (make-b->b)))
  (print (b->b-x b))
  (print (b->b-y b)))