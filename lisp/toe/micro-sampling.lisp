
(defun micro-sampling (tbl n)
  ; get the number of classes and number of instances in this table
  (setf class-no (length (discrete-uniques (first(last(table-columns tbl)))))
        table-length  (length (mapcar #'eg-features (table-all tbl)))
	table-elements  (mapcar #'eg-features (table-all tbl)))


  ; define a hash table to put the class elements into
  (setf class-hash (make-hash-table :test #'equal))
 
  (dotimes (counter table-length)
    ; put elements of each class in a table into a hash table
    ; key will be class label and map will be list of elements of this class
    (setf selected (nth counter table-elements))
    (setf return-value  (gethash (last selected) class-hash))
    (if (not (listp (first return-value))) 
	(setf (gethash (last selected) class-hash) (append (list return-value) (list selected)))
	(setf (gethash (last selected) class-hash) (append return-value (list selected))))      
 )
  (dohash (key value class-hash)
    (dotimes (i n)
    (format t "~d => ~d~%" i (nth (my-random-int (length value)) value))  
    ))
)
    
(deftest test-micro-sampling ()
  "Return a n random samples of each class in a table"
  (micro-sampling (weather-numerics) 10))
