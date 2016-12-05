(defstruct stage 
  pos
  counter
  )



(defun NextStageHeur (st act) 
  (let ((pos (list (+ (first (stage-pos st)) (first act) ) (+ (second (stage-pos st)) (second act) ) ) )) 
  (let(( st2 (make-STAGE :POS pos 
        :COUNTER (1+ (stage-other st)) )))
  st2
  )))


;;; Pedir 
(defun NextStagesHeur (st)
  "generate all possible next states"
  (let ((lst '()))
  (dolist (el (possible-actions))
    (setf lst (cons  (nextStageHeur st el) lst )) )
  lst)
  )


;; Heuristic
(defun compute-heuristic (st)
  
  (let(( st2 (make-STAGE :POS (state-pos st) 
        :COUNTER 0 ))
  (cond ((IsGoalp st) 0)
    ( (IsObstaclep (state-pos st) (state-track st)) most-positive-fixnum )
    ( t (compute-heuristic-aux (list st) 0))
	)
  ))
  

(defun compute-heuristic-aux(lst index)
  (let ((result '()))
  (let ((current (nth index lst)))
    (let ((currentList (NextStatesHeur current)))
    (dolist(el currentList)
      (cond ((isObstaclep (state-pos el) (state-track el)) (setf currentList (remove el currentList)))
        ((isGoalp el) (return-from compute-heuristic-aux (stage-counter el)) )
        (t ( dolist(ell lst)
          (if (equal (state-pos el) (state-pos ell))  
            (setf currentList(remove el currentList))
            )
          )
        )
      )
    )
    (setf result (append lst currentList))
    (compute-heuristic-aux result (1+ index)) 
  )
  )
  )
  )