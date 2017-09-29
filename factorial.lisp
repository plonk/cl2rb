(defun fact (n)
  (if (= n 0)
      1
    (* n (fact (1- n)))))

(print (fact 10))