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
      (setf openList (remove expansionNode openList))
      (push expansionNode closedList)
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (print "bem campeaao")
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
        )
  (print "1")
  (loop while (not (eq (list-length openList) 0))
    do (let*  ( (expansionNode (findLowestF openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      (setf openList (remove expansionNode openList))
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from bestsearch3 (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (pos (state-pos st8))
                (h (nth (second pos) (nth (first pos) Hmap)))
                )
            (when (not (null (nth (second pos) (nth (first pos) Hmap)) )) (push (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ) openList)
            )
          )
        )

      )

  )
  (print "hi")
  (return-from bestsearch3 nil)
)
)
)




(defun findLowestF (lista)

  (let* ( (minimumF most-positive-fixnum)
          (finalNode nil)
          (listaF '())
        )
  (dolist (el lista)
    (setf listaF (append (list (node-f el)) listaF))
    (when (< (node-f el) minimumF) 
        (setf minimumF (node-f el))
        (setf finalNode el)
      )
    )
  ;;;(print minimumF)
  ;;;(print (state-pos (node-state finalNode)))
  ;;;(print (state-vel (node-state finalNode)))
  ;;(print listaF)
  ;;(print minimumF)
  (return-from findLowestF finalNode)
)
)





(defun bestsearchHEAP (problem)
  (let*((Htrack (copy-structure (state-track (problem-initial-state problem))))
        (Hmap (track-env Htrack))
        

    ) 
  (fillmap2 Htrack)
  (let* ( 
          (initpos (state-pos (problem-initial-state problem)))
          (heur (nth (second initpos) (nth (first initpos) Hmap)) )
          (openList (make-heap :size 0))     
        )
  (insertNode (make-node  :state (problem-initial-state problem)
                      :g 0
                      :h heur
                      :f heur) openList )
  (print "loop")
  (loop while (not (equal (heap-size openList) 0))
    do (let*  ( (expansionNode (extractMin openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      ;;(print (state-pos (node-state expansionNode)))
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from bestsearchHEAP (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (pos (state-pos st8))
                (h (nth (second pos) (nth (first pos) Hmap)))
                )
            (when (not (null (nth (second pos) (nth (first pos) Hmap)) )) (insertNode (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ) openList)
            )
          )
        )

      )

  )
  (print "hi")
  (return-from bestsearchHEAP nil)
)
)
)




(defun bestsearchQ (problem)
  (let*((Htrack (copy-structure (state-track (problem-initial-state problem))))
        (Hmap (track-env Htrack))
        

    ) 
  (fillmap2 Htrack)
  (let* ( 
          (initpos (state-pos (problem-initial-state problem)))
          (heur (nth (second initpos) (nth (first initpos) Hmap)) )
          (openList (make-q 
                  )
          )     
        )
  (enqueue-by-priority openList (make-node  :state (problem-initial-state problem)
                      :g 0
                      :h heur
                      :f heur
                      ) (q-key openList))
  ;;(print openList)
  ;;kjahsdkhasd
  (loop while (not (empty-queue? openList))
    do (let*  ( (expansionNode (remove-front openList))
                (nextSt (funcall(problem-fn-nextStates problem) (node-state expansionNode)))
              )
      ;;(print (state-pos (node-state expansionNode)))
      (when (funcall(problem-fn-isGoal problem) (node-state expansionNode))
        (return-from bestsearchQ (solution expansionNode))
        )
      (dolist (st8 nextSt)
        (let* ( (g (+ (node-g expansionNode) (state-cost st8)))
                (pos (state-pos st8))
                (h (nth (second pos) (nth (first pos) Hmap)))
                )
            (when (not (null (nth (second pos) (nth (first pos) Hmap)) )) (enqueue-by-priority openList (make-node  
                      :parent expansionNode
                      :state st8
                      :g g
                      :h h
                      :f (+ g h)
                      ) (q-key openList) )
            )
          )
        )

      )

  )
  (print "hi")
  (return-from bestsearchQ nil)
)
)
)





(defstruct heap 
   nodes
   size)


(defun insertNode (node hip)
  (when (null (heap-nodes hip))
      (setf (heap-nodes hip) (make-array 50 :fill-pointer 0 :adjustable t)))
  (vector-push-extend node (heap-nodes hip))
    (incf (heap-size hip))
    (print "fuck you")
    (bubbleUp hip node)
  )


(defun bubbleUp (hip node)
  (let ((pos (1- (heap-size hip))))
  (loop while (and (> pos 0) (>= (node-f (aref (heap-nodes hip) (/ pos 2)))  (node-f (aref pos (heap-nodes hip)))))
    do (setf (aref (heap-nodes hip) pos) (aref (heap-nodes hip) (/ pos 2)))
      (setf pos (/ pos 2))
      
  )
  (setf (aref pos (heap-nodes hip)) node)
)
)

(defun extractMin (hip)
  (let ((result (aref (heap-nodes hip) 0 )))
  (setf (aref (heap-nodes hip) 0) (aref (heap-nodes hip) (1- (heap-size hip))  )  )
  (decf (fill-pointer (heap-nodes hip)))
  (decf (heap-size hip) )
  (sinkdown 0 hip)
  (result)

  )
  )

(defun sinkdown (pos hip)
  (let ((minChild))
  (if (> (node-f (aref (heap-nodes hip) (* pos 2))) (node-f (aref (heap-nodes hip) (1+ (* pos 2)))) ) 
    (setf minChild (1+ (* pos 2)))
    (setf minChild (* pos 2)) 
    )
  (when (> (node-f (aref (heap-nodes hip) pos)) (node-f (aref (heap-nodes hip) minChild) ) )
    (let ((y (aref (heap-nodes hip) pos)))
      (setf   (aref (heap-nodes hip) pos)  (aref (heap-nodes hip) minChild)  )
      (setf   (aref  (heap-nodes hip) minChild) y)
      )
    (sinkdown minChild hip)
    )

  )
)





;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/queue.lisp

;;;; The Queue datatype

;;; We can remove elements form the front of a queue.  We can add elements in
;;; three ways: to the front, to the back, or ordered by some numeric score.
;;; This is done with the following enqueing functions, which make use of the
;;; following implementations of the elements:
;;;   ENQUEUE-AT-FRONT - elements are a list
;;;   ENQUEUE-AT-END   - elements are a list, with a pointer to end
;;;   ENQUEUE-BY-PRIORITY - elements are a heap, implemented as an array
;;; The best element in the queue is always in position 0.

;;; The heap implementation is taken from "Introduction to Algorithms" by
;;; Cormen, Lieserson & Rivest [CL&R], Chapter 7.  We could certainly speed
;;; up the constant factors of this implementation.  It is meant to be clear
;;; and simple and O(log n), but not super efficient.  Consider a Fibonacci
;;; heap [Page 420 CL&R] if you really have large queues to deal with.

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

;;;; Basic Operations on Queues

(defun make-empty-queue () (make-q))

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))

;;;; The Three Enqueing Functions

(defun enqueue-at-front (q items)
  "Add a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q))))

(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (cond ((null items) nil)
  ((or (null (q-last q)) (null (q-elements q)))
   (setf (q-last q) (last items)
         (q-elements q) (nconc (q-elements q) items)))
  (t (setf (cdr (q-last q)) items
     (q-last q) (last items)))))

(defun enqueue-by-priority (q items key)
  "Insert the items by priority according to the key function."
  ;; First make sure the queue is in a consistent state
  (setf (q-key q) key)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
    (heap-insert (q-elements q) items key))

;;;; The Heap Implementation of Priority Queues

;;; The idea is to store a heap in an array so that the heap property is
;;; maintained for all elements: heap[Parent(i)] <= heap[i].  Note that we
;;; start at index 0, not 1, and that we put the lowest value at the top of
;;; the heap, not the highest value.

;; These could be made inline

(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  
  (let ((l (heap-left i))
  (r (heap-right i))
  (N (- (length heap) 1))
  smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
           (heap-val heap i key)))
           l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
  (setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  "Pop the best (lowest valued) item off the heap. [Page 150 CL&R]."
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

;;(heap-insert (q-elements q) items key))
(defun heap-insert (heap item key)
  "Put an item into a heap. [Page 150 CL&R]."
  ;; Note that ITEM is the value to be inserted, and KEY is a function
  ;; that extracts the numeric value from the item.
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
  (val (funcall key item)))
    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val))
      do (setf (aref heap i) (aref heap (heap-parent i))
         i (heap-parent i)))
    (setf (aref heap i) item)))

;;(defun make-heap (&optional (size 100))
  ;;(make-array size :fill-pointer 0 :adjustable t))

(defun heap-sort (numbers &key (key #'identity))
  "Return a sorted list, with elements that are < according to key first."
  ;; Mostly for testing the heap implementation
  ;; There are more efficient ways of sorting (even of heap-sorting)
  (let ((heap (make-heap))
  (result nil))
    (for each n in numbers do (heap-insert heap n key))
    (loop while (> (length heap) 0) do (push (heap-extract-min heap key) result))
    (nreverse result)))



(defun identity (item)
  (return-from identity (node-f item)))