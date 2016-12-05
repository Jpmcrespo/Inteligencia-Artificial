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

(setf *t1* (loadtrack "ntrack1.txt"))

(format t "~&Exercise 3.1 - Heuristic~&")
(with-open-file (str "out3.1.txt"
     :direction :input)
  (format t "~% Solution is correct? ~a~&" (equal (list (compute-heuristic (initial-state *t1*)) (compute-heuristic (make-state :pos '(1 6)  :track track)) (compute-heuristic (make-state :pos '(2 8)  :track track))) (read str))))
  
