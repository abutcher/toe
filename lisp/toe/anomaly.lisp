(defun anomaly-detector (train tests file &optional (fix-anomaly? NIL))
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create )
    (let* ((seen (count-list train))
	   (train-mm (train-min-max (mapcar #'eg-features (egs train)) seen))
	   (train-min (first train-mm))
	   (train-max (second train-mm)))
      
      (format stream "TRAINING ON: ~A~%" (table-name train))
      (format stream "TRAIN LENGTH: ~A~%" (length (egs train)))
      
      (dolist (this-test tests)
	(format stream "~%~%TESTING ON ERA: ~A~%" (position this-test tests))
	(format stream "ERA LENGTH: ~A~%" (length (egs this-test)))
	(let ((accepted)(rejected))
	  (dolist (eg (mapcar #'eg-features (egs this-test)))
	    (let ((eg-likelyhood (likelyhood eg seen)))
	      (if (and (< eg-likelyhood train-min)
		       (> eg-likelyhood train-max))
		  (push eg rejected)
		  (push eg accepted))))
	  (format stream "ACCEPTED: ~A~%" (length accepted))
	  (format stream "REJECTED: ~A~%" (length rejected))

	  ; If user set fix-anomaly's to true we'll fix them here
	  (if fix-anomaly?
	      (progn
		(dolist (eg rejected)
		  (push (contrast eg train) accepted))
		(format stream "PATCHED ~A ANOMALIES~%" (length rejected))))

	  ; Update train table
	  (setf train 
		(data
		 :name (table-name train)
		 :columns (columns-header (table-columns train))
		 :klass (table-class train)
		 :egs (append accepted (mapcar #'eg-features (egs train)))
		 ))
	  ; Remake seen
	  (setf seen (count-list train))
	  ; Update train-min and train-max
	  (setf train-mm (train-min-max (mapcar #'eg-features (egs train)) seen))
	  (setf train-min (first train-mm))
	  (setf train-max (second train-mm)))))
    file))

(defun train-min-max (egs seen)
  (let (l)
    (dolist (eg egs)
      (push (likelyhood eg seen) l))
    (find-min-max l)))