(setq i "global")

(print
 (do ((i 0 (+ 1 i))
      j)
     ((> i 10) i)
     (print (list i j))
     (setq j i)
     ))

(print i)

(print "====")

(print
 (do ()
     (t)
     (print "a")
     ))

(print "====")
