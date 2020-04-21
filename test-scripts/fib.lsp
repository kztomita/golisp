(defun fib (n)
  (if (or (= n 0) (= n 1))
      n
    (+ (fib (- n 2))
       (fib (- n 1)))))

(print (fib 0))
(print (fib 1))
(print (fib 2))
(print (fib 3))
(print (fib 4))
(print (fib 5))
(print (fib 6))
(print (fib 7))
