(defun normalize (tbl)
  "Computes (value - min)/(max - min) for all numeric cols"
  (let ((trans (super-transform tbl)))
    (dolist (column trans)
      (if (numericp (first column))
	  (progn 
	    (let* ((min-max (find-min-max (rest column)))
		   (min (first min-max))
		   (max (second min-max)))
	      (setf (rest column)
		    (mapcar #'(lambda (value) 
				(/ (- value min) (- max min))) (rest column))))
	    )))
    (setf trans (transpose trans))
    (data
     :name (table-name tbl)
     :columns (car trans)
     :klass (table-class tbl)
     :egs (cdr trans)
     )))

(deftest test-normal ()
  "Return a normalized table"
  (normalize (weather-numerics)))

(defun apply-minmax (tbl)
  (let ((trans (super-transform tbl)) (new-cols (table-columns tbl)))
    (dolist (attr new-cols)
      (if (numeric-p attr)
	  (setf (numeric-mm attr)
		(find-min-max 
		 (cdr (nth (position attr new-cols) trans))))))
    (setf (table-columns tbl) new-cols)
    tbl))


(defun normalize-mod (tbl)
  "Computes (value - min)/(max - min) for all numbers but rounds to nearest tenth"
  (let ((trans (super-transform tbl)))
    (dolist (column trans)
      (if (numericp (first column))
	  (progn 
	    (let* ((min-max (find-min-max (rest column)))
		   (min (first min-max))
		   (max (second min-max)))
	      (setf (rest column)
		    (mapcar #'(lambda (value) 
				(/ (fround (/ (- value min) (- max min)) .1) 10)) (rest column))))
	    )))
    (setf trans (transpose trans))
    (let ((new-table
	   (data
	    :name (table-name tbl)
	    :columns (car trans)
	    :klass (table-class tbl)
	    :egs (cdr trans)
	    )))
      (setf (table-columns new-table) (table-columns tbl))
      new-table)))

(defun remove-num-headers (tbl)
  (data
   :name (table-name tbl)
   :columns (mapcar #'(lambda (x)
			(intern
			 (replace-all
			  (string x) "$" "")))
			(columns-header (table-columns tbl)))
   :klass (table-class tbl)
   :egs (mapcar #'eg-features (egs tbl))))

; Run as: (normalize-mod (apply-minmax (weather-numerics)))