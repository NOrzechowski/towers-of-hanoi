; working with lists

(car '(dog chicken cat horse goat))

(cdr '(dog chicken cat horse goat))
(append '(dog chicken) '(cat horse goat))
(cons '(dog chicken) '(cat horse goat))

; math
(+ 99 1)

; functions
(defun fib (num)
   (cond ((<= num 1) num)
     (t
       ( + (fib (- num 1)) (fib (- num 2)))
     )
   )
)


(fib 6)