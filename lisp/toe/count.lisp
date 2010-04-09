(defun count-tot (tbl)
  "Consolidates xindex hash tables into a single table"
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (attr (table-columns (xindex tbl)))
      (if (numeric-p attr)
	  (dohash (key value (numeric-f attr))
	    (setf key (push (numeric-name attr) key))
	    (setf (gethash key ht) value))
	  (dohash (key value (discrete-f attr))
	    (setf key (reverse key))
	    (setf key (push (discrete-name attr) key))
	    (setf (gethash key ht) value))))
    ht))

(defun count-list (tbl)
  (let (seen)
    (dolist (attr (table-columns (xindex tbl)))
      (if (numeric-p attr)
	  (push (numeric-f attr) seen)
	  (push (discrete-f attr) seen)))
    (reverse seen)))

(deftest test-xindex-tot ()
  (dohash (key value (count-tot (weather-numerics)))
    (format t "~A ~A~%" key value)))