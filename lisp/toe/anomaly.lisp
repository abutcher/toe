(defun anomaly-detector (train tests file &key (fix-anomaly? NIL) (drop-what 5))
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create )
    (let* ((seen (xindex-tot-2 train))
	   (train-mm (train-min-max (mapcar #'eg-features (egs train)) seen drop-what))
	   (train-min (first train-mm))
	   (train-max (second train-mm))
	   (train-spread (third train-mm))
	   (accept-count 0)
	   (reject-count 0)
	   (test-spread))
      
      (format stream "TRAINING ON: ~A~%" (table-name train))
      (format stream "TRAIN LENGTH: ~A~%" (length (egs train)))
      
      (dolist (this-test tests)
	(format stream "~%~%TESTING ON ~A ERA: ~A~%"
		(table-name this-test) (position this-test tests))
	(format stream "ERA LENGTH: ~A~%" (length (egs this-test)))
	(let (accepted rejected)
	  (dolist (eg (mapcar #'eg-features (egs this-test)))
	    (let ((eg-likelyhood (likelyhood eg seen)))
	      (if (or (< eg-likelyhood train-min)
		       (> eg-likelyhood train-max))
		  (push eg rejected)
		  (progn 
		    (unless (null eg-likelyhood)
		      (push eg-likelyhood test-spread))
		    (push eg accepted)))))
	  
	  (format stream "ACCEPTED: ~A~%" (length accepted))
	  (format stream "REJECTED: ~A~%" (length rejected))

	  ; Update global accept/reject ratio
	  (setf accept-count (+ accept-count (length accepted)))
	  (setf reject-count (+ reject-count (length rejected)))

	  ; If user set fix-anomaly's to true we'll fix them here
	  ; and update any globals that need fixing
	  (if fix-anomaly?
	      (progn
		(let ((sampled-table (sampler train 1))(save-rejected (copy-list rejected)))
		  (dolist (eg rejected)
		    (let ((new-node (contrast (copy-list eg) sampled-table train-min train-max seen)))
		      (push new-node accepted)
		      (push (likelyhood new-node seen) test-spread)
		      (format stream "NEW-NODE: ~A~%" new-node)))
		  (format stream "REJECTED LIST: ~A~%" save-rejected)
		  (format stream "PATCHED ~A ANOMALIES~%" (length rejected))
		  (setf accept-count (+ accept-count (length rejected)))
		  (setf reject-count (- reject-count (length rejected))))))

	  (if (null (first (last test-spread)))
		(setf test-spread (remove nil test-spread)))

	  ; Update train table
	  (unless (null test-spread)
;	    (print train-spread)
;	    (print test-spread)
	    (if (= 0 (ttest-from-lists train-spread test-spread))
		(progn 
		  (format stream "~%~% CHOOSING TO ACCEPT ERA ~A ~%" (position this-test tests))
		  (setf train 
			(data
			 :name (table-name train)
			 :columns (columns-header (table-columns train))
			 :klass (table-class train)
			 :egs (append accepted (mapcar #'eg-features (egs train)))
			 ))
		  ; Remake seen
		  (setf seen (xindex-tot-2 train))
		  ; Update train-min and train-max
		  (setf train-mm (train-min-max (mapcar #'eg-features (egs train)) seen drop-what))
		  (setf train-min (first train-mm))
		  (setf train-max (second train-mm))
		  (setf train-spread (third train-mm))
		  (setf test-spread '(())))))))
      (format stream "OVERALL ACCEPTED/REJECTED: ~A/~A~%" accept-count reject-count))
    file))

(defun train-min-max (egs seen drop-what)
  (let (l)
    (dolist (eg egs)
      (push (likelyhood eg seen) l))
    (let* ((train-mm (find-min-max l))
	   (train-min (first train-mm))
	   (train-max (second train-mm))
	   (diff (/ (- train-max train-min) (/ 100 drop-what)))
	   (train-min (+ train-min diff)))
;	   (train-max (- train-max diff)))
      (list train-min train-max l))))
      
(defun give-me-1000 (tbl)
  (let ((tbl-egs (mapcar #'eg-features (egs (randomizer tbl)))))
    (data
     :name (table-name tbl)
     :columns (columns-header (table-columns tbl))
     :klass (table-class tbl)
     :egs (subseq tbl-egs 0 1000)
     )
    ))
