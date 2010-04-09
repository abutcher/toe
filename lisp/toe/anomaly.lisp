(defun anomaly-detector (train test file)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create )
    (let ((seen (count-list train))
	  (test-egs (mapcar #'eg-features (egs test)))
	  (train-egs (mapcar #'eg-features (egs train)))
	  (test-tmp)
	  (train-tmp)
	  (train-minmax)
	  (test-minmax))
      
      (format stream "TRAINING ON: ~A~%" (table-name train))
      (format stream "TESTING ON: ~A~%" (table-name test))	

      (dolist (eg train-egs)
	(push (list eg (likelyhood eg seen)) train-tmp))      
      (dolist (eg test-egs)
	(push (list eg (likelyhood eg seen)) test-tmp))
      
      (format stream "********* TRAIN SET *********~%")
      (setf train-minmax (find-min-max 
			  (mapcar #'first (mapcar #'cdr train-tmp))))
      (setf train-min (first train-minmax))
      (setf train-max (second train-minmax))
      (format stream "MIN: ~A MAX: ~A~%" train-min train-max)
      (format stream "~A~%" train-tmp)
      
      (format stream "********* TEST SET *********~%")
      (setf test-minmax (find-min-max 
			 (mapcar #'first (mapcar #'cdr test-tmp))))
      (setf test-min (first test-minmax))
      (setf test-max (second test-minmax))
      (format stream "MIN: ~A MAX: ~A~%" test-min test-max)
      (format stream "~A~%" test-tmp)
      
      (let ((accept 0) (reject 0))
	(dolist (eg test-tmp)
	  (if (or (< (first (cdr eg)) train-min)
		  (> (first (cdr eg)) train-max))
	      (incf reject)
	      (incf accept)))
	(format stream "ACCEPTED: ~A REJECTED: ~A~%" accept reject))
      file)))

(defun run-tests ()
  (anomaly-detector 
   (sampler (weather-yes-train) 100) 
   (sampler (weather-yes-normal-test) 30) 
   "weather-yes-normal.txt")

  (anomaly-detector 
   (sampler (weather-no-train) 100) 
   (sampler (weather-no-normal-test) 30) 
   "weather-no-normal.txt")

  (anomaly-detector 
   (sampler (weather-yes-train) 100) 
   (sampler (weather-yes-abnormal-test) 30) 
   "weather-yes-abnormal.txt")

  (anomaly-detector 
   (sampler (weather-no-train) 100) 
   (sampler (weather-no-abnormal-test) 30) 
   "weather-no-abnormal.txt")

  (anomaly-detector 
   (sampler (weather-numerics) 100) 
   (sampler (weather-yes-normal-test) 30) 
   "weather-vs-yes-normal.txt")

  (anomaly-detector 
   (sampler (weather-numerics) 100) 
   (sampler (weather-yes-abnormal-test) 30) 
   "weather-vs-yes-abnormal.txt")

  (anomaly-detector 
   (sampler (weather-numerics) 100) 
   (sampler (weather-no-normal-test) 30) 
   "weather-vs-no-normal.txt")

  (anomaly-detector 
   (sampler (weather-numerics) 100) 
   (sampler (weather-no-abnormal-test) 30) 
   "weather-vs-no-abnormal.txt"))