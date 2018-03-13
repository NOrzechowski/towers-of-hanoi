# Towers of Hanoi
# Goal: to move 'n' discs from peg 'A' to peg 'C'
# Restrictions: Cannot put a bigger disc ontop of a smaller disc
# Algorithm:
## 1. move n-1 dics from A -> B
## 2. move disc n from A -> C
## 3. move n-1 dics from B -> C


(defun initialize-start-tower(start end)
  (loop for x from start to end
    collect x into return-list
    finally (return-from initialize-start-tower (reverse return-list))
  )
)

(defun get-tower(towers i) 
  (nth (- i 1) towers)
)

(defun initialize-towers(n)
  (setq tower-one  (initialize-start-tower 1 n))
  (list tower-one '() '())
)

(defun reconstruct-towers(source aux dest)
  (list source aux dest)
)

(defun update-tower(towers i tower)
  (cond
    ((= i 1) (reconstruct-towers tower (get-tower towers 2) (get-tower towers 3)))
    ((= i 2) (reconstruct-towers  (get-tower towers 1) tower (get-tower towers 3)))
    ((= i 3) (reconstruct-towers (get-tower towers 1) (get-tower towers 2) tower))
  )
)    

(defun get-top(towers i)
  (setq param (get-tower towers i))
  (pop param)
)

(defun pop-top(towers i)
  (setq param (get-tower towers i))
  (update-tower towers i (cdr param))
 )

(defun push-top(towers i disk)
  (setq param (get-tower towers i))
  (update-tower towers i (push disk param))
)

(defun move-disk(source dest towers) 
  (let 
    ((top-disk (get-top towers source)) 
      (towers-tmp (pop-top towers source)))
    (push-top towers-tmp dest top-disk)
   )
  )

               
 (defun hanoi-solver(n source aux dest towers)
  (format t "params: [~a] [~a] [~a] [~a] [~a] ~%" n source aux dest towers)
  (cond
   ((> n 0)
   (hanoi-solver (- n 1) source dest aux towers)
   (move-disk source dest towers)
   (format t "move [~a] from [~a] to [~a] ~%" (nth 0 (get-tower towers 1)) source dest)
   (hanoi-solver (- n 1) aux source dest towers)
   )
  )
)
                   


(defun towers-of-hanoi(n)
  (hanoi-solver n 1 2 3 (initialize-towers n))
)

(towers-of-hanoi 3)


(setq towers '((3 2 1) NIL NIL))
(move-disk 1 2 towers)

