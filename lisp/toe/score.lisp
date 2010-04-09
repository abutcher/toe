(defun score-bore (tbl colnum &key (best 0.2) (op #'>))
  (let*  ((trans (super-transform tbl))
	  (score-column (rest (copy-list (nth colnum trans))))
	  (tmp-column (copy-list score-column))
	  (times (floor (* (length score-column) best)))
	  threshold)
    
    (if (numberp (first score-column))
	(progn 
	  (setf tmp-column (sort tmp-column op))
	  (setf threshold (nth (- times 1) tmp-column))
	  (setf score-column 
		(mapcar #'(lambda (x) 
			    (if (or (funcall op x threshold)
				    (= x threshold))
				'BEST
				'REST))
			score-column))
	 (setf score-column (cons 'BORE score-column)))
	(progn
	  (format t "~%Specified column ~A in table ~A is not numeric~%~%" 
		  (first (nth colnum trans)) (table-name tbl))
	  (setf score-column nil)))
    
    score-column))

(defun score-class (tbl)
  (let*  ((trans (super-transform tbl))
	  (score-column (rest (nth (table-class tbl) trans))))
    (cons 'CLASS score-column)))
