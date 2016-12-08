;;; this is your solution file
(load "SolF3.lisp")

(defun states-to-list (stts)
  (loop for st in stts
	  collect (format nil "POS: ~a VEL: ~a ACT: ~a COST: ~a~&"
	  (state-pos st)  (state-vel st)  (state-action st)  (state-cost st))))

(defun initial-state (track)
  (make-state :pos (track-startpos track) :vel (make-vel 0 0) :action nil :cost 0 :track track))

(defvar *t1* nil)
(defvar *p1* nil)

(setf *t1* (loadtrack "track0.txt"))

(setf *p1* (make-problem :initial-state (initial-state *t1*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
			  
	
(defvar *t2* nil)
(defvar *p2* nil)

(setf *t2* (loadtrack "ntrack1.txt"))

(setf *p2* (make-problem :initial-state (initial-state *t2*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'vector-distance))
			  
(format t "~&Exercise 3.2b - BestSearch")
(print (time (states-to-list (bestsearch *p2*))))

	