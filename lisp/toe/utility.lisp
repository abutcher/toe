(defun utility (tbl score-column)
  (let*  ((trans (super-transform tbl))
	  class-index)    
    (if (null score-column)
	()
	(progn
	  (setf trans (append trans (list score-column)))    
	  (setf class-index (- (length trans) 1))
	  (setf trans (transpose trans))
	  (data
	   :name (table-name tbl)
	   :columns (car trans)     
	   :egs (cdr trans)
	   :klass class-index )))))

(deftest test-utility ()
  "Testing utility"
  (format t "~%#####################Utility as BORE#####################~%~%")
  (print (utility (weather-numerics) (score-bore (weather-numerics) 1)))
  (format t "~%#####################Utility with class as score#####################~%~%")
  (print (utility (weather-numerics) (score-class (weather-numerics)))))
