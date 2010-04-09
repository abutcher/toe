(defun discrete-entropy (attr instance seen)
  (let ((numerator 0)(denominator 0)(entropy 0))
    (dohash (key value (nth (position attr instance) seen))
      (if (member attr instance)
	  (progn
	    (setf numerator (+ numerator value))
	    (setf denominator (+ denominator value)))
	  (setf denominator (+ denominator value))))
    (setf entropy (* (/ numerator demoninator) 
		     (/ (log (/ numerator denominator)) (log 2))))
    entropy))

(defun numeric-entropy (attr instance seen)

)
            
	       
      