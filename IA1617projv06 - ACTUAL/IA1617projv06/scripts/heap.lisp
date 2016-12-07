(defstruct heap 
   nodes
   size)


(defun insertNode (node hip)
	(when (null (heap-nodes hip))
    	(setf (heap-nodes hip) (make-array 50 :fill-pointer 0 :adjustable t)))
	(vector-push-extend node (heap-elements hip))
  	(incf (heap-size hip))
  	(bubbleUp hip)
  )


(defun bubbleUp (hip node)
  (let ((pos (1- (heap-size hip))))
  (loop while (and (> pos 0) (>= (node-f (aref (heap-nodes hip) (/ pos 2)))  (node-f (aref pos (heap-nodes hip)))))
    do ((setf (aref (heap-nodes hip) pos) (aref (heap-nodes hip) (/ pos 2)))
    	(setf pos (/ pos 2))
      )
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