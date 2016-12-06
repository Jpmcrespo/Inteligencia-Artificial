(load "SolF3.lisp")

(setf *t1* (loadtrack "ntrack1.txt"))

;;(initializeTrack *t1*)

;;(print *t1*)

(time(fillmap2 *t1*))

(print *t1*)
