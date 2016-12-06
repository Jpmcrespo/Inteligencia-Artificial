(load "datastructures.lisp")
(load "auxfuncs.lisp")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))
 

(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
  (track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
   T)))



(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
    (make-vel (+ (vel-l (state-vel st)) (acce-l act))
        (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
    (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
        (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
    (cond ((isGoalp new-state) -100)
    ((isObstaclep (state-pos new-state) (state-track new-state)) 20)
    (T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
              (pos-c (state-pos st)))))
    (values new-state)))
;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
  (let ((successors nil))
    (dolist (act (possible-actions) successors)
      (let ((new-state (nextState st act)))
  (if (not (member new-state successors :test #'equalp))
      (push new-state successors))))))

;;; Solucao e uma seq ordenada de estados
(defun solution (node)
  (let ((seq-states nil))
    (loop 
      (when (null node)
  (return))
      (push (node-state node) seq-states)
      (setf node (node-parent node)))
    (values seq-states)))


;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim &key cutoff?)

  (labels ((limdepthfirstsearch-aux (node problem lim)
       (if (isGoalp (node-state node))
     (solution node)
     (if (zerop lim)
         :cutoff
         (let ((cutoff? nil))
           (dolist (new-state (nextStates (node-state node)))
       (let* ((new-node (make-node :parent node :state new-state))
        (res (limdepthfirstsearch-aux new-node problem (1- lim))))
         (if (eq res :cutoff)
             (setf cutoff? :cutoff)
             (if (not (null res))
           (return-from limdepthfirstsearch-aux res)))))
           (values cutoff?))))))
    (let ((res (limdepthfirstsearch-aux (make-node :parent nil :state (problem-initial-state problem))
          problem
          lim)))
      (if (eq res :cutoff)
    (if cutoff?
        :cutoff
        nil)
    res))))
              

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
 
  (let ((i 0))
    (loop
      (let ((res (limdepthfirstsearch problem i :cutoff? T)))
  (when (and res (not (eq res :cutoff)))
    (return res))
  (incf i)
  (if (> i lim)
      (return nil))))))


;; Solution of phase 3




(defun NextStateHeur (st act) 
  (let ((pos (list (+ (first (state-pos st)) (first act) ) (+ (second (state-pos st)) (second act) ) ) )) 
  (let(( st2 (make-STATE :POS pos 
        :VEL 0
        :ACTION act
        :COST 0
        :TRACK (state-track st)
        :OTHER (1+ (state-other st)) )))
  st2
  )))


;;; Pedir 
(defun NextStatesHeur (st)
  "generate all possible next states"
  (let ((lst '()))
  (dolist (el (possible-actions))
    (setf lst (cons  (nextStateHeur st el) lst )) )
  lst)
  )


(defun NextPos (pos act) 
(list (+ (first pos) (first act) ) (+ (second pos) (second act) ) ) 
  
) 


(defun NextPoses (pos)
(let ((lst '()))
  (dolist (el (possible-actions))
    (setf lst (cons  (nextPos pos el) lst )) )
  lst)  
  )


;; Heuristic

(defun compute-heuristic (st)
  
  (setf (state-other st) 0)
  (cond ((IsGoalp st) 0)
    ( (IsObstaclep (state-pos st) (state-track st)) most-positive-fixnum )
    ( t (compute-heuristic-aux (list st) 0 (state-track st)))
  )
  )

  

(defun compute-heuristic-aux(lst index trak)
  (let ((result '()))
  (let ((current (nth index lst)))
    (let ((currentList (NextStatesHeur current)))
    (dolist(el currentList)
      (cond ((isObstaclep (state-pos el) trak) (setf currentList (remove el currentList)))
        ((isGoalp el) (return-from compute-heuristic-aux (state-other el)) )
        (t ( dolist(ell lst)
          (if (equal (state-pos el) (state-pos ell))  
            (setf currentList(remove el currentList))
            )
          )
        )
      )
    )
    (setf result (append lst currentList))
    (compute-heuristic-aux result (1+ index) trak) 
  )
  )
  )
  )



(defun pos-equal (a b)
  (return-from pos-equal (and (eq (first a) (first b) ) (eq (second a) (second b) ) ) )
  )



(defun initializeTrack(trak)
  (let* ((Htrack (track-env trak))
        (i 0)
        (e 0)
        )

    (dolist (outterList Htrack)
      (dolist (element outterList)
        (cond ((null element) (setf (nth e (nth i Htrack)) most-positive-fixnum ))
          )

        (incf e)
        )
      (setf e 0)
      (incf i)
      )
    Htrack)
  )

(defun fillmap (track)
  (let* (( i 0 )
          (currentList (track-endpositions track))
          (oldList '())
          (newList '())
          (nextPosList '())
          (Htrack (track-env track) )
          )
  (loop while (not (eq (list-length currentList) 0))
    do (dolist (pos currentList)
      (setf (nth (second pos) (nth (first pos) Htrack)) i )
      (setf nextPosList (NextPoses pos))

      (dolist (testpos nextPosList)
        (when  (and (not(isObstaclep testpos track)) (not (member testpos oldList :test #'pos-equal)) (not (member testpos (track-endpositions track ) :test #'pos-equal )) )
              
              (push testpos newList)
              (push testpos oldList)
          )
      )    
   )
  (setf currentList newList)
  (setf newList '())
  (incf i)
  )

  ))


(defun fillmap2(track)
  (let* (( i 0 )
          (currentList (track-endpositions track))
          (newList '())
          (nextPosList '())
          (Htrack (track-env track) )
          )
  (dolist (pos currentList)
    (setf (nth (second pos) (nth (first pos) Htrack)) i )
    )
  (incf i)
  (loop while (not (eq (list-length currentList) 0))
    do (dolist (pos currentList)
      (setf nextPosList (NextPoses pos))
      (dolist (testpos nextPosList)
        (when  (and (equal t (nth (second testpos) (nth (first testpos) Htrack))) )
              (setf (nth (second testpos) (nth (first testpos) Htrack)) i )
              (push testpos newList)
          )
      )    
   )
  (setf currentList newList)
  (setf newList '())
  (incf i)
  )

  )
  )

(defun vector-distance (st) ;;cena do silveira
  (setf result most-positive-fixnum)
  (let ((track  (state-track st)))
        (dolist (endpos (track-endpositions track))
          (let* ((distCol  (- (second endpos) (second (state-pos st))))
          (distLin  (- (first  endpos) (first (state-pos st)))))
          (setf result (min result (+ distCol (max (- distLin distCol) 0))))
          )
          )

  (return-from vector-distance result)

  )
)


;;; A*
(defun a* (problem)
  (let* ( (heur (funcall(problem-fn-h problem) (problem-initial-state problem)))
          (openList (list (make-node  :state (problem-initial-state problem)
                      :g 0
                      :h heur
                      :f heur
                      )
                  )
          )
        )
  (loop while (not (eq (list-length openList) 0))
    do (let*  ( (expansionNode (findLowestF openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      (setf openList (remove expansionNode openList))
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from a* (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (h (funcall(problem-fn-h problem) st8))
                (successor (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ))
                )
            
            (push successor openList)  
          )
        )

      )

  )
  (return-from a* nil)
)
)



;;; bestsearch
(defun bestsearch (problem)
  (let((Htrack (copy-structure (state-track (problem-initial-state problem))))) 

  (fillmap2 Htrack)

  (let* ( 
          (Hmap (track-env Htrack))

          (initpos (state-pos (problem-initial-state problem)))
          (heur (nth (second initpos) (nth (first initpos) Hmap)) )
          (openList (list (make-node  :state (problem-initial-state problem)
                      :g 0
                      :h heur
                      :f heur
                      )
                  )
          )
          (closedList '())
          
        )
  (print "1")
  (loop while (not (eq (list-length openList) 0))
    do (let*  ( (expansionNode (findLowestF openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      (print (state-pos (node-state expansionNode)))
      (setf openList (remove expansionNode openList))
      (push expansionNode closedList)
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from bestsearch (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (pos (state-pos st8))
                (h (nth (second pos) (nth (first pos) Hmap)))
                (successor (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ))
                (flag nil)
                )
            (dolist (oldNode (append openList closedList))
              (when (equal (state-pos (node-state oldNode)) (state-pos (node-state successor)))
                (setf flag t) (return))
              )
            (when (not flag) (push successor openList))  
          )
        )

      )

  )
  (return-from bestsearch nil)
)
)
)




(defun bestsearch2 (problem)
  (let((Htrack (copy-structure (state-track (problem-initial-state problem))))) 

  (fillmap2 Htrack)
  (let* ( (Hmap (track-env Htrack))

          (initpos (state-pos (problem-initial-state problem)))
          (heur (nth (second initpos) (nth (first initpos) Hmap)) )
          (openList (list (make-node  :state (problem-initial-state problem)
                      :g 0
                      :h heur
                      :f heur
                      )
                  )
          )
        )
  (loop while (not (eq (list-length openList) 0))
    do (let*  ( (expansionNode (findLowestF openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      (setf openList (remove expansionNode openList))
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from bestsearch2 (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (pos (state-pos st8))
                (h (nth (second pos) (nth (first pos) Hmap)))
                (successor (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ))
                )
            
            (push successor openList)  
          )
        )

      )

  )
  (return-from bestsearch2 nil)
)
))



(defun bestsearch3 (problem)
  (let*((Htrack (copy-structure (state-track (problem-initial-state problem))))
        (Hmap (track-env Htrack))
        (closedMap (copy-structure Hmap))

    ) 

  (fillmap2 Htrack)

  (let* ( 
          

          (initpos (state-pos (problem-initial-state problem)))
          (heur (nth (second initpos) (nth (first initpos) Hmap)) )
          (openList (list (make-node  :state (problem-initial-state problem)
                      :g 0
                      :h heur
                      :f heur
                      )
                  )
          )
          (closedList '())
          
        )
  (print "1")
  (loop while (not (eq (list-length openList) 0))
    do (let*  ( (expansionNode (findLowestF openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      (print (state-pos (node-state expansionNode)))
      (setf openList (remove expansionNode openList))
      (push expansionNode closedList)
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from bestsearch (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (pos (state-pos st8))
                (h (nth (second pos) (nth (first pos) Hmap)))
                (successor (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ))
                (flag nil)
                )
            (dolist (oldNode (append openList closedList))
              (when (equal (state-pos (node-state oldNode)) (state-pos (node-state successor)))
                (setf flag t) (return))
              )
            (when (not flag) (push successor openList))
          )
        )

      )

  )
  (return-from bestsearch nil)
)
)
)




(defun findLowestF (lista)
  (let* ( (minimumF most-positive-fixnum)
          (finalNode nil)
        )
  (dolist (el lista)
    (when (< (node-f el) minimumF) 
        (setf minimumF (node-f el))
        (setf finalNode el)
      )
    )
  ;;;(print minimumF)
  ;;;(print (state-pos (node-state finalNode)))
  ;;;(print (state-vel (node-state finalNode)))

  (return-from findLowestF finalNode)
)
)

