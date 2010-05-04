(defun run-test (train-tbl anom-tbl drop-what)
  (let ((combo (discretize (combine-egs
			    (sampler train-tbl 900)
			    (sampler anom-tbl 100)) :n 10)))
    (anomaly-detector
     combo
     (era (sampler (discretize train-tbl :n 10) 1100) :n 100)
     (format nil "mod-tests/~A-vs-normal-drop~A.txt" (table-name train-tbl) drop-what)
     :drop-what drop-what)
    (anomaly-detector
     combo
     (era (sampler (discretize anom-tbl :n 10) 1100) :n 100)
     (format nil "mod-tests/~A-vs-anomalous-drop~A.txt" (table-name train-tbl) drop-what)
     :drop-what drop-what)))

(defun run-tests (tests drop-what)
  (dolist (test tests)
    (let ((train (first test))
	  (anom (second test)))
      (run-test (funcall train) (funcall anom) drop-what))))

(defparameter *TESTS*
  (list 
   (list 'breast-cancer-train 'breast-cancer-anom)
   (list 'contact-lens-train 'contact-lens-anom)
   (list 'credit-rating-train 'credit-rating-anom)
   (list 'cpu-train 'cpu-anom)
   (list 'cloud-train-2 'cloud-anom-2)
   (list 'cleveland-14-heart-disease-train 'cleveland-14-heart-disease-anom)
   (list 'ionosphere-train 'ionosphere-anom)))

(defun run-em-all-bitch ()
  (dolist (drop '(10 5 20 2.5 25))
    (run-tests *TESTS* drop)
    (run-big-tests drop)))

(defun run-big-tests (drop-what)
  (anomaly-detector (discretize (combine-egs
				 (give-me-n (mushroom-train) 900)
				 (give-me-n (mushroom-anom) 100)) :n 10)
		    (era (discretize (give-me-n (mushroom-train) 1100) :n 10) :n 100)
		    (format nil "mod-tests/MUSHROOM-train-vs-normal-drop~A.txt" drop-what)
		    :drop-what drop-what)
  (anomaly-detector (discretize (combine-egs
				 (give-me-n (mushroom-train) 900)
				 (give-me-n (mushroom-anom) 100)) :n 10)
		    (era (discretize (give-me-n (mushroom-anom) 1100) :n 10) :n 100)
		    (format nil "mod-tests/MUSHROOM-train-vs-anomalous-drop~A.txt" drop-what)
		    :drop-what drop-what)
  (anomaly-detector (discretize (combine-egs
				 (give-me-n (splice-train) 900)
				 (give-me-n (splice-anom) 100)) :n 10)
		    (era (discretize (give-me-n (splice-train) 1100) :n 10) :n 100)
		    (format nil "mod-tests/SPLICE-train-vs-normal-drop~A.txt" drop-what)
		    :drop-what drop-what)
  (anomaly-detector (discretize (combine-egs
				 (give-me-n (splice-train) 900)
				 (give-me-n (splice-anom) 100)) :n 10)
		    (era (discretize (give-me-n (splice-anom) 1100) :n 10) :n 100)
		    (format nil "mod-tests/SPLICE-train-vs-anomalous-drop~A.txt" drop-what)
		    :drop-what drop-what)
  (anomaly-detector (discretize (combine-egs
				 (give-me-n (kr-vs-kp-train) 900)
				 (give-me-n (kr-vs-kp-anom) 100)) :n 10)
		    (era (discretize (give-me-n (kr-vs-kp-train) 1100) :n 10) :n 100)
		    (format nil "mod-tests/KR-VS-KP-train-vs-normal-drop~A.txt" drop-what)
		    :drop-what drop-what)
  (anomaly-detector (discretize (combine-egs
				 (give-me-n (kr-vs-kp-train) 900)
				 (give-me-n (kr-vs-kp-anom) 100)) :n 10)
		    (era (discretize (give-me-n (kr-vs-kp-anom) 1100) :n 10) :n 100)
		    (format nil "mod-tests/KR-VS-KP-train-vs-anomalous-drop~A.txt" drop-what)
		    :drop-what drop-what)
  )
