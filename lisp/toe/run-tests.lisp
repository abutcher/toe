(defun run-test (train-tbl anom-tbl drop-what)
  (let ((combo (combine-egs
		(sampler (discretize train-tbl :n 10) 900)
		(sampler (discretize anom-tbl :n 10) 100))))
    (anomaly-detector
     combo
     (era (sampler (discretize train-tbl :n 10) 1000) :n 100)
     (format nil "mod-tests/~A-vs-normal-drop~A.txt" (table-name train-tbl) drop-what)
     :drop-what drop-what)
    (anomaly-detector
     combo
     (era (sampler (discretize anom-tbl :n 10) 1000) :n 100)
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