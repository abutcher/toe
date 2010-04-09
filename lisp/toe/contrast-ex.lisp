(defun contrast (bad-eg good-table)
  (let* ((good-egs (mapcar #'eg-features (egs good-table)))
	 (good-eg 
	  (random-element 
	   (mapcar #'eg-features (egs (sampler good-table 1)))))
	 (good-seen (xindex-tot-2 good-table))
	 (good-list)
	 (good-range)
	 (good-min 0)
	 (good-max 0)
	 (bad-mitigations)
	 (bad-score 0))

    (dolist (eg good-egs)
      (push (list eg (likelyhood eg good-seen)) good-list))

    (setf good-range
	  (find-min-max 
	   (mapcar #'first (mapcar #'cdr good-list))))
    
    (setf good-min (first good-range))
    (setf good-max (second good-range))

    (loop while (= bad-score 0) do
	 (let ((this-eg bad-eg)
	       (this-attr (random-element bad-eg)))
	   (setf (nth (position this-attr bad-eg) this-eg) (nth (position this-attr bad-eg) good-eg))
	   (setq bad-score (likelyhood this-eg good-seen))
	   (if (and (> bad-score good-min) (< bad-score good-max))
	       (push this-eg bad-mitigations))))
    (car bad-mitigations)
    ))

(deftest test-contrast ()
  (format t "This guy is bad: '(RAINY 5000 7500 TRUE YES)~%")
  (format t "This guy is good: ~A~%" 
	  (contrast '(RAINY 5000 7005 TRUE YES) (weather-numerics))))