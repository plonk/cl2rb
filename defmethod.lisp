(defstruct a)
(defstruct (b (:include a)))

(defmethod f ((x a) (y a))
  (print "A-A"))

(defmethod f ((x b) (y a))
  (print "B-A")
  (call-next-method))

(defmethod f ((x a) (y b))
  (print "A-B")
  (call-next-method))

(defmethod f ((x b) (y b))
  (print "B-B")
  (call-next-method))

(f (make-b) (make-b))
;; "B-B"
;; "B-A"
;; "A-B"
;; "A-A" 

