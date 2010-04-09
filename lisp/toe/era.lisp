(defun era (tbl &key (offset 1) (n 1))
  (let* ((egx (append (list (columns-header (table-columns tbl))) (mapcar #'eg-features (egs tbl))))
    (era-list (nth 0 egx))
    era-tmp)      
      (dotimes (i n)
	(setf era-tmp (append (list (nth (+ i offset) egx)) era-tmp)))
      (setf era-list (append era-tmp (list era-list)))
      (setf era-list (reverse era-list))

      (data
       :name (table-name tbl)
       :columns (car era-list)
       :klass (table-class tbl)
       :egs (cdr era-list))))

(deftest test-era ( &key (eranum 2) (erasize 2))
  (dotimes (i eranum)
    (format t "~%**************ERA #~A***************~%" i)
    (print (era (weather-numerics) :offset (+ i 1) :n erasize))))
