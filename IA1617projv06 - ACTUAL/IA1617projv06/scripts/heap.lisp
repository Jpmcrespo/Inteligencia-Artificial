(defstruct heap
   nodes
   size)


(defun insertNode (node hip)

  (when (null (heap-nodes hip))
      (setf (heap-nodes hip) (make-array 50 :fill-pointer 0 :adjustable t)))
  (vector-push-extend nil (heap-nodes hip))
  (incf (heap-size hip))
  (bubbleUp hip node)

  )



(defun heapKeyValue (hip pos) (node-f (aref (heap-nodes hip) pos)))
(defun heapParent (pos)  (floor (/ (1- pos) 2)))

(defun bubbleUp (hip node)
  (let ((pos (1- (heap-size hip))))
  (loop while (and (> pos 0) (>= (heapKeyValue hip (heapParent pos))  (node-f node)))
    do (setf (aref (heap-nodes hip) pos) (aref (heap-nodes hip) (heapParent pos))
       pos (heapParent pos)) )
  (setf (aref  (heap-nodes hip) pos) node)))

(defun extractMin (hip)
  
  (let ((result (aref (heap-nodes hip) 0 )))
  (setf (aref (heap-nodes hip) 0) (aref (heap-nodes hip) (1- (heap-size hip))))
  (decf (fill-pointer (heap-nodes hip)))
  (decf (heap-size hip) )
  (sinkdown 0 hip)
  result))

(defun sinkdown (pos hip)
  (let ((l (1+ (* pos 2)))
        (r (+ 2 (* pos 2)))
        (largest pos))
    (when (and (<= l (heap-size hip)) (<= (heapKeyValue hip l) (heapKeyValue hip largest)))
      (setf largest l))
    (when (and (<= r (heap-size hip)) (<= (heapKeyValue hip r) (heapKeyValue hip largest)))
      (setf largest r))
    (when (not (equal largest pos))
      (rotatef (aref (heap-nodes hip) pos) (aref (heap-nodes hip) largest))
      (sinkdown largest hip))))
