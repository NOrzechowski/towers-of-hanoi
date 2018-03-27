; Towers of Hanoi
; Restrictions: Cannot put a bigger disc on top of a smaller disc
; Basic algorithm:
; - move n-1 dics from 1 -> 2
; - move disc n from 1 -> 3
; - move n-1 dics from 2 -> 3

(defun initialize-start-tower(start end)
  (loop for x from start to end
    collect x into return-list
    finally (return-from initialize-start-tower return-list)
  )
)

(defun get-tower(towers i) 
  (nth (- i 1) towers)
)

(defun initialize-towers(n)
  (setq tower-one  (initialize-start-tower 1 n))
  (list tower-one '() '())
)

(defun replace-tower(towers i tower)
  (cond
    ((= i 1) (list tower (get-tower towers 2) (get-tower towers 3)))
    ((= i 2) (list (get-tower towers 1) tower (get-tower towers 3)))
    ((= i 3) (list (get-tower towers 1) (get-tower towers 2) tower))
  )
)    

(defun get-top(towers i)
  (setq param (get-tower towers i))
  (pop param)
)

(defun pop-top(towers i)
  (setq param (get-tower towers i))
  (replace-tower towers i (cdr param))
)

(defun push-top(towers i disk)
  (setq param (get-tower towers i))
  (replace-tower towers i (push disk param))
)
; moves the top disk from 'source' 
; to 'dest', and returns the new state
(defun move-disk(source dest towers) 
  (let 
    (
     (top-disk (get-top towers source))
     (towers-tmp (pop-top towers source))
    )
    (format t "~% move [~a] from [~a] to [~a]" top-disk source dest) 
    (push-top towers-tmp dest top-disk)
  )
)

; Main recursive function 
; Paramters:
;	n      - number of disks
; 	source - source tower, integer	
;	aux    - auxillary tower, integer
;	dest   - destination tower, integer
;	towers - current state of towers                
 (defun hanoi-solver(n source aux dest towers)
  (cond
   ((= n 1)
     (move-disk source dest towers))
     (t
       (hanoi-solver (- n 1) aux source dest 
         (move-disk source dest
         (hanoi-solver (- n 1) source dest aux towers))			
       )
     )
  )
)

; recursive wrapper
(defun towers-of-hanoi(n)
  (setq towers (initialize-towers n))
  (format t "~% Initial tower configuration: ~a" towers)
  (format t "~% Final tower configuration: ~a" (hanoi-solver n 1 2 3 towers))
)

; runner
(format t "~% Enter Number of Disks: ")
(setq n (read))
(cond
  ((> n 10)
    (format t "~% Too big."))
    (t 
      (towers-of-hanoi n)
    )
)
