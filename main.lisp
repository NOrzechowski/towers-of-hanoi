# Towers of Hanoi
# Goal: to move 'n' discs from peg 'A' to peg 'C'
# Restrictions: Cannot put a bigger disc ontop of a smaller disc
# Algorithm:
## 1. move n-1 dics from A -> B
## 2. move disc n from A -> C
## 3. move n-1 dics from B -> C

(defun integer-sequence(start end)
  (loop for x from start to end
    collect x)
  )


(defun hanoi-solver(n source dest aux)
  (cond
   ((> n 0)
   (hanoi-solver (- n 1) source aux dest)
   (append dest (pop source))
    (format t "Source: [~a] , Aux: [~a], Dest: [~a] ~%" source aux dest)
   (hanoi-solver (- n 1) aux dest source)
   )
  )
 )


(defun towers-of-hanoi(n)
  (setq int-list (reverse (integer-sequence 1 n)))
  (hanoi-solver n int-list '() '())
   (print int-list)
  )

(towers-of-hanoi 3)