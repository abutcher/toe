(defun anomaly-detector (train tests file &optional (fix-anomaly? NIL))
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create )
    (let* ((seen (xindex-tot-2 train))
	   (train-mm (train-min-max (mapcar #'eg-features (egs train)) seen))
	   (train-min (first train-mm))
	   (train-max (second train-mm))
	   (accept-count 0)
	   (reject-count 0))
      
      (format stream "TRAINING ON: ~A~%" (table-name train))
      (format stream "TRAIN LENGTH: ~A~%" (length (egs train)))
      
      (dolist (this-test tests)
	(format stream "~%~%TESTING ON ~A ERA: ~A~%" 
		(table-name this-test) (position this-test tests))
	(format stream "ERA LENGTH: ~A~%" (length (egs this-test)))
	(let ((accepted)(rejected))
	  (dolist (eg (mapcar #'eg-features (egs this-test)))
	    (let ((eg-likelyhood (likelyhood eg seen)))
	      (if (or (< eg-likelyhood train-min)
		       (> eg-likelyhood train-max))
		  (push eg rejected)
		  (push eg accepted))))
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
		      (format stream "NEW-NODE: ~A~%" new-node)))
		  (format stream "REJECTED LIST: ~A~%" save-rejected)
		  (format stream "PATCHED ~A ANOMALIES~%" (length rejected))
		  (setf accept-count (+ accept-count (length rejected)))
		  (setf reject-count (- reject-count (length rejected))))))

	  ; Update train table
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
	  (setf train-mm (train-min-max (mapcar #'eg-features (egs train)) seen))
	  (setf train-min (first train-mm))
	  (setf train-max (second train-mm))))
      (format stream "OVERALL ACCEPTED/REJECTED: ~A/~A~%" accept-count reject-count))
    file))

(defun train-min-max (egs seen)
  (let (l)
    (dolist (eg egs)
      (push (likelyhood eg seen) l))
    (let* ((train-mm (find-min-max l))
	   (train-min (first train-mm))
	   (train-max (second train-mm))
	   (diff (/ (- train-max train-min) 20))
	   (train-min (+ train-min diff)))
;	   (train-max (- train-max diff)))
      (list train-min train-max))))
      
(defun run-tests ()
  (anomaly-detector 
   (sampler (discretize (contact-lens-train-90-10) :n 10) 1000)
   (era (sampler (discretize (contact-lens-anom) :n 10) 1000) :n 100)
   "contact-lens-train-vs-anom.txt")

  (anomaly-detector 
   (sampler (discretize (contact-lens-train-90-10) :n 10) 1000)
   (era (sampler (discretize (contact-lens-train-90-10)) 1000) :n 100)
   "contact-lens-train-vs-normal.txt")

  (anomaly-detector 
   (sampler (discretize (credit-rating-train-90-10) :n 10) 1000)
   (era (sampler (discretize (credit-rating-anom) :n 10) 1000) :n 100)
   "credit-rating-train-vs-anom.txt")

  (anomaly-detector 
   (sampler (discretize (credit-rating-train-90-10) :n 10) 1000)
   (era (sampler (discretize (credit-rating-train-90-10) :n 10) 1000) :n 100)
   "credit-rating-vs-normal.txt")

  (anomaly-detector 
   (sampler (discretize (breast-cancer-train-90-10) :n 10) 1000)
   (era (sampler (discretize (breast-cancer-anom) :n 10) 1000) :n 100)
   "breast-cancer-train-vs-anom.txt")
  
  (anomaly-detector 
   (sampler (discretize (breast-cancer-train-90-10)) 1000)
   (era (sampler (discretize (breast-cancer-train)) 1000) :n 100)
   "breast-cancer-train-vs-normal.txt")

  (anomaly-detector 
   (sampler (discretize (cpu-train-90-10) :n 10) 1000)
   (era (sampler (discretize (cpu-anom) :n 10) 1000) :n 100)
   "cpu-train-vs-anom.txt")

  (anomaly-detector 
   (sampler (discretize (cpu-train-90-10) :n 10) 1000)
   (era (sampler (discretize (cpu-train-90-10) :n 10) 1000) :n 100)
   "cpu-train-vs-normal.txt")

;  (anomaly-detector 
;   (sampler (discretize (german_credit-train-90-10) :n 10) 1000)
;   (era (sampler (discretize (german_credit-anom) :n 10) 1000) :n 100)
;   "german_credit-train-vs-anom.txt")

;  (anomaly-detector 
;   (sampler (discretize (german_credit-train-90-10) :n 10) 1000)
;   (era (sampler (discretize (german_credit-train-90-10) :n 10) 1000) :n 100)
;   "german_credit-train-vs-normal.txt")

  (anomaly-detector 
   (sampler (discretize (cloud-train-2-90-10) :n 10) 1000)
   (era (sampler (discretize (cloud-anom-2) :n 10) 1000) :n 100)
   "cloud-train-vs-anom.txt")
  
  (anomaly-detector 
   (sampler (discretize (cloud-train-2-90-10) :n 10) 1000)
   (era (sampler (discretize (cloud-train-2-90-10) :n 10) 1000) :n 100)
   "cloud-train-vs-normal.txt")
  
  (anomaly-detector 
   (sampler (discretize (cleveland-14-heart-disease-train-90-10) :n 10) 1000)
   (era (sampler (discretize (cleveland-14-heart-disease-anom) :n 10) 1000) :n 100)
   "cleveland-14-heart-disease-train-vs-anom.txt")

  (anomaly-detector 
   (sampler (discretize (cleveland-14-heart-disease-train-90-10) :n 10) 1000)
   (era (sampler (discretize (cleveland-14-heart-disease-train-90-10) :n 10) 1000) :n 100)
   "cleveland-14-heart-disease-train-vs-normal.txt")

  (anomaly-detector 
   (sampler (discretize (ionosphere-train-90-10) :n 10) 1000)
   (era (sampler (discretize (ionosphere-anom) :n 10) 1000) :n 100)
   "ionosphere-train-vs-anom.txt")

  (anomaly-detector 
   (sampler (discretize (ionosphere-train-90-10) :n 10) 1000)
   (era (sampler (discretize (ionosphere-train-90-10) :n 10) 1000) :n 100)
   "ionosphere-train-vs-normal.txt")

  (anomaly-detector 
   (sampler (discretize (kr-vs-kp-train-90-10) :n 10) 1000)
   (era (sampler (discretize (kr-vs-kp-anom) :n 10) 1000) :n 100)
   "kr-vs-kp-train-vs-anom.txt")

  (anomaly-detector 
   (sampler (discretize (kr-vs-kp-train-90-10) :n 10) 1000)
   (era (sampler (discretize (kr-vs-kp-train-90-10) :n 10) 1000) :n 100)
   "kr-vs-kp-train-vs-normal.txt")

  (anomaly-detector
   (discretize (give-me-1000 (mushroom-train-90-10)) :n 10)
   (era (discretize (give-me-1000 (mushroom-anom)) :n 10) :n 100)
   "mushroom-train-vs-anom.txt")

  (anomaly-detector
   (discretize (give-me-1000 (mushroom-train-90-10)) :n 10)
   (era (discretize (give-me-1000 (mushroom-train-90-10))) :n 100)
   "mushroom-train-vs-normal.txt")

  (anomaly-detector 
   (discretize (give-me-1000 (splice-train-90-10)))
   (era (discretize (give-me-1000 (splice-anom)) :n 10) :n 100)
   "splice-train-vs-anom.txt")

  (anomaly-detector 
   (discretize (give-me-1000 (splice-train-90-10)) :n 10)
   (era (discretize (give-me-1000 (splice-train-90-10)) :n 10) :n 100)
   "splice-train-vs-normal.txt")
)

(defun give-me-1000 (tbl)
  (let ((tbl-egs (mapcar #'eg-features (egs (randomizer tbl)))))
    (data
     :name (table-name tbl)
     :columns (columns-header (table-columns tbl))
     :klass (table-class tbl)
     :egs (subseq tbl-egs 0 1000)
     )
    ))

(defun run-tests-with-fix ()
  (anomaly-detector 
   (sampler (discretize (contact-lens-train)) 1000)
   (era (sampler (discretize (contact-lens-anom)) 1000) :n 100)
   "contact-lens-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (contact-lens-train)) 1000)
   (era (sampler (discretize (contact-lens-train)) 1000) :n 100)
   "contact-lens-train-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (credit-rating-train)) 1000)
   (era (sampler (discretize (credit-rating-anom)) 1000) :n 100)
   "credit-rating-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (credit-rating-train)) 1000)
   (era (sampler (discretize (credit-rating-train)) 1000) :n 100)
   "credit-rating-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (breast-cancer-train)) 1000)
   (era (sampler (discretize (breast-cancer-anom)) 1000) :n 100)
   "breast-cancer-train-vs-anom-with-fix.txt" T)
  
  (anomaly-detector 
   (sampler (discretize (breast-cancer-train)) 1000)
   (era (sampler (discretize (breast-cancer-train)) 1000) :n 100)
   "breast-cancer-train-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (cpu-train)) 1000)
   (era (sampler (discretize (cpu-anom)) 1000) :n 100)
   "cpu-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (cpu-train)) 1000)
   (era (sampler (discretize (cpu-train)) 1000) :n 100)
   "cpu-train-vs-normal-with-fix.txt" T)

;  (anomaly-detector 
;   (sampler (discretize (german_credit-train)) 1000)
;   (era (sampler (discretize (german_credit-anom)) 1000) :n 100)
;   "german_credit-train-vs-anom.txt" T)

;  (anomaly-detector 
;   (sampler (discretize (german_credit-train)) 1000)
;   (era (sampler (discretize (german_credit-train)) 1000) :n 100)
;   "german_credit-train-vs-normal.txt" T)

  (anomaly-detector 
   (sampler (discretize (cloud-train)) 1000)
   (era (sampler (discretize (cloud-anom)) 1000) :n 100)
   "cloud-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (cloud-train)) 1000)
   (era (sampler (discretize (cloud-train)) 1000) :n 100)
   "cloud-train-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (cleveland-14-heart-disease-train)) 1000)
   (era (sampler (discretize (cleveland-14-heart-disease-anom)) 1000) :n 100)
   "cleveland-14-heart-disease-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (cleveland-14-heart-disease-train)) 1000)
   (era (sampler (discretize (cleveland-14-heart-disease-train)) 1000) :n 100)
   "cleveland-14-heart-disease-train-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (ionosphere-train)) 1000)
   (era (sampler (discretize (ionosphere-anom)) 1000) :n 100)
   "ionosphere-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (ionosphere-train)) 1000)
   (era (sampler (discretize (ionosphere-train)) 1000) :n 100)
   "ionosphere-train-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (kr-vs-kp-train)) 1000)
   (era (sampler (discretize (kr-vs-kp-anom)) 1000) :n 100)
   "kr-vs-kp-train-vs-anom-with-fix.txt" T)

  (anomaly-detector 
   (sampler (discretize (kr-vs-kp-train)) 1000)
   (era (sampler (discretize (kr-vs-kp-train)) 1000) :n 100)
   "kr-vs-kp-train-vs-normal-with-fix.txt" T)

  (anomaly-detector
   (discretize (give-me-1000 (mushroom-train)))
   (era (discretize (give-me-1000 (mushroom-anom))) :n 100)
   "mushroom-train-vs-anom-with-fix.txt" T)

  (anomaly-detector
   (discretize (give-me-1000 (mushroom-train)))
   (era (discretize (give-me-1000 (mushroom-train))) :n 100)
   "mushroom-train-vs-normal-with-fix.txt" T)

  (anomaly-detector 
   (discretize (give-me-1000 (splice-train)))
   (era (discretize (give-me-1000 (splice-anom))) :n 100)
   "splice-train-vs-anom.txt" T)

  (anomaly-detector 
   (discretize (give-me-1000 (splice-train)))
   (era (discretize (give-me-1000 (splice-train))) :n 100)
   "splice-train-vs-normal.txt" T)
)




