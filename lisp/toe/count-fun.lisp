(defun count-fun (tbl)
  ; get the number of classes and number of instances in this table
  (setf class-no (length (discrete-uniques (first(last(table-columns tbl)))))
        table-length  (length (mapcar #'eg-features (table-all tbl)))
	table-elements  (mapcar #'eg-features (table-all tbl)))
  ; define a hash table to put the class elements into
  (setf count-hash (make-hash-table :test #'equal))
  (dotimes (counter table-length)
    ; put each attribute/range/class combination into hash-map
    (setf selected (nth counter table-elements))
    (setf return-value  (gethash selected count-hash))
    (if (eql nil return-value) 
	(setf (gethash selected count-hash) (append return-value (list selected)))
	(setf (gethash selected count-hash) (append return-value (list selected)))))      
  ; traverse the hashmap and write the counts
  (setf my-count 0)
  (dohash (key value count-hash)
    (if (> (length value) 0)
	(format t "Attribute/range/class> ~d Count => ~d~%" 
		(first value)  
		(length value)))))

(deftest test-count-fun ()
  "Return frequencies of all attribute/range/class"
  (count-fun (weather-numerics)))

(defun count-fun-no-nums (tbl)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (eg (mapcar #'eg-features (egs tbl)))
      (let (hash)
	(dolist (el eg)
	  (if (not (numberp el))
	      (push el hash)))
	(setf hash (reverse hash))
	(if (null (gethash hash ht))
	    (setf (gethash hash ht) 1)
	    (incf (gethash hash ht)))))
    ht))

(deftest test-count-fun-no-nums ()
  (dohash (key value (count-fun-no-nums (weather-numerics)))
	   (format t "~A ~A~%" key value)))