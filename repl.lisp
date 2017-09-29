(defun repl ()
    (princ (eval (read)))
    (terpri)
    (repl))

(repl)
