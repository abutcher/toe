(defun sampler (tbl n)
  (let ((gac-tree (gac tbl))
	(samples))
    (labels ((walk (tree)
	       (let ((visit (first tree))
		     (right (second tree))
		     (left (third tree))
		     (redhead (fourth tree)))
		 (if (and (not (null right)) 
			  (listp (car right)) 
			  (>= (length right) 3))
		     (walk right)
		     (if (not (null right))
			 (push (favored-median visit right
					       (my-random-int (/ n 10))
					       (my-random-int (/ n 10)))
			       samples)))
		 (if (and (not (null left)) 
			  (listp (car left)) 
			  (>= (length left) 3))
		     (walk left)
		     (if (not (null left))
			 (push (favored-median visit left
					       (my-random-int (/ n 10))
					       (my-random-int (/ n 10)))
			       samples)))
		 (if (and (not (null redhead)) 
			  (listp (car redhead)) 
			  (>= (length redhead) 3))
		     (walk redhead)
		     (if (not (null redhead))
			 (push (favored-median visit redhead
					       (my-random-int (/ n 10))
					       (my-random-int (/ n 10)))
			       samples))))))
      (loop
	 (if (>= (length samples) n) (return))
	 (walk gac-tree)))
    (data
     :name (table-name tbl)
     :columns (columns-header (table-columns tbl))
     :klass (table-class tbl)
     :egs (subseq samples 0 n)
     )))

(deftest test-sampler ()
  (print (sampler (weather-numerics))))

(defun leaves (gtree-node)
  (let ((leaf-list))
    (labels ((walk (tree)
	       (let ((visit (first tree))
		     (right (second tree))
		     (left (third tree))
		     (redhead (fourth tree)))
		 (if (and (not (null right)) 
			  (listp (car right))
			  (>= (length right) 3))
		     (walk right)
		     (if (not (null right))
			 (push right leaf-list)))
		 (if (and (not (null left)) 
			  (listp (car left))
			  (>= (length left) 3))
		     (walk left)
		     (if (not (null left))
			 (push left leaf-list)))
		 (if (and (not (null redhead))
			  (listp (car redhead))
			  (>= (length redhead) 3))
		     (walk redhead)
		     (if (not (null redhead))
			 (push redhead leaf-list))))))
      (walk gtree-node))
    leaf-list))

(defun class-counts (t-klass)
  "Generates the counts of classes in a list. 
   Feed it (mapcar #'eg-class (egs tbl))"
  (let ((l))
    (dolist (k t-klass l)
      (if (equal nil (assoc k l))
	  (push (cons k 1) l)
	  (setf (cdr (assoc k l)) (1+ (cdr (assoc k l))))))))

(defun combine-egs (tbl-one tbl-two)
  (let* ((tbl-one-egs (mapcar #'eg-features (egs tbl-one)))
	 (tbl-two-egs (mapcar #'eg-features (egs tbl-two)))
	 (combined-egs (append tbl-one-egs tbl-two-egs)))
    (data
     :name (table-name tbl-one)
     :columns (columns-header (table-columns tbl-one))
     :klass (table-class tbl-one)
     :egs combined-egs
     )))