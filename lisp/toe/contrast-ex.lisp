(defun contrast (bad-eg sampled-table good-min good-max good-seen)
  (let* ((good-eg 
	  (random-element 
	   (mapcar #'eg-features (egs sampled-table))))
	 (bad-mitigations)
	 (bad-score 0)
	 (orig-eg (copy-list bad-eg)))
    (loop while (or (< bad-score good-min) (> bad-score good-max)) do
	 (let ((this-eg bad-eg)
	       (this-attr (random-element bad-eg)))
	   (setf (nth (position this-attr bad-eg) this-eg) (nth (position this-attr bad-eg) good-eg))
	   (setq bad-score (likelyhood this-eg good-seen))
	   (print this-eg)
	   (if (not (eql this-eg orig-eg))
	       (if (and (> bad-score good-min) (< bad-score good-max))
		   (push this-eg bad-mitigations)))))
    (car bad-mitigations)
    ))

(deftest test-contrast ()
  (let* ((seen (xindex-tot-2 (discretize (weather-yes-train))))
	 (train-mm
	  (train-min-max 
	   (mapcar #'eg-features 
		   (egs (discretize (weather-yes-train)))) seen))
	 (train-min (first train-mm))
	 (train-max (second train-mm)))
    (contrast '(RAINY 0 2 TRUE CHEESE) 
	      (sampler (discretize (weather-yes-train)) 1)
	      train-min
	      train-max
	      seen)))
	 
	   
		    
