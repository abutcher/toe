(defun gac (tbl)
  "Returns a tree-list of clusters where the root is the centroid"
  (let ((k (table-class tbl))
	(rows (copy-list (mapcar #'eg-features (egs tbl))))
	(tree))
    (dotimes (i (floor (/ (length rows) 2)))
      (let* ((one (car rows))
	     (two (propose one (cdr rows) k)))
	(setf rows (delete one rows))
	(setf rows (delete two rows))
	(if (= (length tree) 1)
	    (push (list (median one two) one two (car tree)) tree)
	    (push (list (median one two) one two) tree))))
    (first (build-gac tree k))))

(defun propose (this those k)
  "Finds the closest record to this out of those"
  (let ((best most-positive-single-float)(ref))
    (dotimes (i (length those))
      (let ((d (distance this (nth i those) k)))
	(if (< d best)(setf best d))
	(push (cons d (nth i those)) ref)))
    (cdr (assoc best ref))))

(defun propose-2 (this those k)
  "Same thing as propose but nested in a list"
  (let ((best most-positive-single-float)(ref))
    (dotimes (i (length those))
      (let ((d (distance (car this) (car (nth i those)) k)))
	(if (< d best)(setf best d))
	(push (cons d (nth i those)) ref)))
    (cdr (assoc best ref))))

(defun build-gac (tree k)
  "Recursively build up the gac tree from the leaves"
  (let ((old-tree (copy-list tree))
	(new-tree))
    (if (or (= (length old-tree) 1) (= (length old-tree) 0))
	old-tree
	(progn
	  (dotimes (i (floor (/ (length old-tree) 2)))
	    (let* ((one (car old-tree))
		   (two (propose-2 one (cdr old-tree) k)))
	      (setf old-tree (delete one old-tree))
	      (setf old-tree (delete two old-tree))
	      (if (= (length old-tree) 1)
		  (push (list (median (car one) (car two)) one two (car old-tree)) new-tree)
		  (push (list (median (car one) (car two)) one two) new-tree))))
	  (build-gac new-tree k)))))

(defun print-dot (gac-tree &optional (stream *standard-output*) (level 0))
  (let ((visit (first gac-tree))
	(right (second gac-tree))
	(left (third gac-tree))
	(redhead (fourth gac-tree)))
    (if (and (not (null right)) (listp (first right)))
	(format stream " \"~A\" -> \"~A\" L=~A;~%" 
		(replace-all (prin1-to-string visit) " " "\\n")
		(replace-all (prin1-to-string (first right)) " " "\\n")
		level)
	(format stream " \"~A\" -> \"~A\" L=~A;~%" 
		(replace-all (prin1-to-string visit) " " "\\n")
		(replace-all (prin1-to-string right) " " "\\n")
		level))
    (if (and (not (null left)) (listp (first left)))
	(format stream " \"~A\" -> \"~A\" L=~A;~%" 
		(replace-all (prin1-to-string visit) " " "\\n")
		(replace-all (prin1-to-string (first left)) " " "\\n")
		level)
	(format stream " \"~A\" -> \"~A\" L=~A;~%" 
		(replace-all (prin1-to-string visit) " " "\\n")
		(replace-all (prin1-to-string left) " " "\\n")
		level))
    (if (and (not (null redhead)) (listp (first redhead)))
	(format stream " \"~A\" -> \"~A\" L=~A;~%" 
		(replace-all (prin1-to-string visit) " " "\\n")
		(replace-all (prin1-to-string (first redhead)) " " "\\n")
		level)
	(if (not (null redhead))
	    (format stream " \"~A\" -> \"~A\" L=~A;~%" 
		    (replace-all (prin1-to-string visit) " " "\\n")
		    (replace-all (prin1-to-string redhead) " " "\\n")
		    level)))
    (if (and (not (null right)) 
	     (listp (first right))
	     (>= (length right) 3)) 
	(print-dot right stream (1+ level)))
    (if (and (not (null left)) 
	     (listp (first left))
	     (>= (length left) 3)) 
	(print-dot left stream (1+ level)))
    (if (and (not (null redhead)) 
	     (listp (first redhead))
	     (>= (length redhead) 3)) 
	(print-dot redhead stream (1+ level)))))

(defun dot (gac-tree file)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create )
    (format stream "DIGRAPH G {~%")
    (print-dot gac-tree stream)
    (format stream "}~%"))
  file)

(deftest test-gac ()
  (let ((gac-tree (gac (weather-numerics))))
    (print gac-tree)
    (dot gac-tree "gac-tree/gac-tree.dot")))