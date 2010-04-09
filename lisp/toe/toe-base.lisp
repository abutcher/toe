(defun numeric-cols (tbl)
  "Gets all the numeric columns from a table."
  (let ((cols))
    (dolist (it (columns-header (table-columns tbl)))
      (if (numericp it)
          (progn
            (setf cols (append cols (list it)))
            (format t "~A is numeric~%" it))))
    cols))

(defun super-transform (tbl)
  "To say this thing is crazy would be an understatement."
  (transpose (append (list (columns-header (table-columns tbl))) (mapcar #'eg-features (egs tbl)))))

(defun find-min-max (numbers)
  "(find-min-max '(1 0 10 4)) returns (0 10)"
  (let ((min most-positive-single-float) (max (* -1 most-positive-single-float)))
    (dolist (i numbers (list min max))
      (if (> i max) (setf max i))
      (if (< i min) (setf min i)))))

(defun target-class (tbl &optional index?)
  "Gets the target class from a table."
  (let ((i 0)
        (target))
    (dolist (item (columns-header (table-columns tbl)))
      (if (eql i (table-class tbl))
	  (setf target (nth i (columns-header (table-columns tbl)))))
      (incf i))
    target))

(defun generic-median (l)
  "median calculated as cieling of ( (max - min) / 2 )"
  (let ((l (sort l #'<)))
    (ceiling (/ (- (nth (- (length l) 1) l) (nth 0 l)) 2))))

(defun random-element (l)
  (nth (my-random-int (length l)) l))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
   is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))