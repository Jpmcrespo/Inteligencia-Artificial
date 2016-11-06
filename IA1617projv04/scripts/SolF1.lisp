
;;; These functions, and any other ones needed must be implemented

(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (null (nth (second pos) (nth (first pos) (track-env track))))
  )

(defun isGoalp (st) 
  "check if st is a goal state"
  (let ((track (state-track st)))
  (dolist (element (track-endpositions track) nil)  
  	(when (equal element (state-pos st))
		(return t)
		)
  	)
  ))



(defun nextState (st act)
  "generate the nextState after state st and action act"
  (let ((vel (list (+ (first (state-vel st)) (first act) ) (+ (second (state-vel st)) (second act) ) ) )) 
  (let ((pos (list (+ (first (state-pos st)) (first vel) ) (+ (second (state-pos st)) (second vel) ) ) )) 
  (let ((track (state-track st)))
  (let(( st2 (make-STATE :POS pos 
	      :VEL vel
	      :ACTION act
	      :COST 0
	      :TRACK (state-track st) )))
  (cond ((isObstaclep pos track) (setf (state-cost st2) 20))
  	((isGoalp st2) (setf (state-cost st2) -100))
  	(t (setf (state-cost st2) 1))
  	)
   st2


  
  )))))


