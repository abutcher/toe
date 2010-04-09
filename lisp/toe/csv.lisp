(defun write-to-csv (tbl file)
  (let ((trans (transpose (super-transform tbl))))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create )
      (format stream "~A~%" (table-name tbl))
      (format stream "~A~%" (table-class tbl))
      (dolist (rec trans)
	(format stream "~A~%" 
		(replace-all (replace-all 
			      (replace-all (prin1-to-string rec) "(" "") ")" "") " " ",")))))
    file)

(defun csv-to-table (file)
  (let ((eggs))
    (with-open-file (stream file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push (string-to-list (replace-all line "," " ")) eggs)))
    (setf eggs (reverse eggs))
    (data
     :name (first (first eggs))
     :klass (first (second eggs))
     :columns (third eggs)
     :egs (subseq eggs 3))))
     

(defun string-to-list (string)
  "Returns a list of the data items represented in the given list."
  (let ((the-list nil) ;; we'll build the list of data items here
        (end-marker (gensym))) ;; a unique value to designate "done"
    (loop (multiple-value-bind (returned-value end-position)
                               (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            ;; if not done, add the read thing to the list
;	    (if (not (null (search "." (write-to-string returned-value))))
;		(setq returned-value (parse-float (write-to-string returned-value))))
            (setq the-list 
                  (append the-list (list returned-value)))
            ;; and chop the read characters off of the string
            (setq string (subseq string end-position))))))

(defun parse-float (str)
  (let ((pos (position #\. str)))
    (if (not pos)
	(parse-integer str)
	(parse-float-help str pos))))

(defun parse-float-help (str pos)
  (let ((new (remove #\. str))
	(len (length str)))
    (let ((int (parse-integer new)))
      (* (/ int (expt 10 (- len pos))) 10))))