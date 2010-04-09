(defun discretize (tbl &key (n 3))
  (let ((trans (super-transform tbl)))
    (dolist (column trans)
      (if (numericp (car column))
	  (progn
	    (let ((scol (sort (copy-list (rest column)) #'<))
		  (bins))
	      (dolist (item scol)
		(push (cons item (round (/ (position item scol) (1+ n)))) bins))
	      (setf (rest column) 
		    (mapcar #'(lambda (item) 
				(cdr (assoc item bins))) (rest column)))))))
    (setf trans (transpose trans))    
    (setf (car trans) 
	  (mapcar #'(lambda (x) 
		      (intern 
		       (replace-all 
			(string x) "$" ""))) 
		  (car trans)))
    (data
     :name (table-name tbl)
     :columns (car trans)
     :klass (table-class tbl)
     :egs (cdr trans)
     )))

(deftest test-discretize ()
  "Return a discretized table"
  (discretize (weather-numerics)))