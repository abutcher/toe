(defun alienator (tbl p)
  "Returns a normal or abnormal table based on probability p"
  (let* ((klass-ratio (class-counts (mapcar #'eg-class (egs tbl)))))
    (if (>= (my-random-int 100) (* 100 (/ 1 p)))
	(sampler tbl)
	(progn
	  (let* ((gac-tree (gac tbl))
		 (gac-root (first gac-tree))
		 (leaf-list (leaves gac-tree)))
	    (dolist (instance leaf-list)
	      (setf (nth (position instance leaf-list) leaf-list) (median gac-root instance)))
	    (dolist (instance leaf-list)
	      (setf (first (last instance)) (nth (my-random-int (length klass-ratio))
						 (mapcar #'first klass-ratio))))
	    (data
	     :name (table-name tbl)
	     :columns (columns-header (table-columns tbl))
	     :klass (table-class tbl)
	     :egs leaf-list
	     ))))))

(deftest test-alienator ()
  (format t "Here's an off table~%")
  (print (alienator (weather-numerics) 1)))
  