(load "SolF3.lisp")

(defun initial-state (track)
  (make-state :pos (track-startpos track) :vel (make-vel 0 0) :action nil :cost 0 :track track))

(setf *t1* (loadtrack "track0.txt"))
(setf *s1* (initial-state *t1*))
(setf *n1* (make-node  :state *s1*
                      :g 0
                      :h 0
                      :f 1))



(setf *t2* (loadtrack "track0.txt"))
(setf *s2* (initial-state *t2*))
(setf *n2* (make-node  :state *s2*
                      :g 0
                      :h 0
                      :f 2))

(setf openList (make-heap :size 0))

(insertNode *n2* openList)
(insertNode *n1* openList)

(setf ext (extractMin openList))

(print openList)