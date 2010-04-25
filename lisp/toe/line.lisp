(defstruct line  x1 y1 x2 y2 m b verticalp)

(defun point-to-line (x1 y1 x2 y2) 
  (if (> x1 x2)
      (point-to-line x2 y2 x1 y1)
      (let* ((rise      (- y2 y1))
	     (run       (- x2 x1))
	     (verticalp (zerop run))
	     m 
	     b)
	(unless verticalp
	  (setf m    (/ rise run)
		b    (- y2 (* m x2))))
	(make-line :x1 x1 :y1  y1 :x2 x2 :y2 y2 
		   :m m :b b :verticalp verticalp))))

(defun line-y (x line)
  (if (line-verticalp line)
      (line-y1 line)
      (+ (* (line-m line)  x) 
	 (line-b line))))

(defun interpolate (x x1 y1 x2 y2 &optional too-big )
  (cond ((<   x x1)   y1)
	((eql x x1)   y1)
	((eql x x2)   y2)
	((< x1 x x2)  (line-y x
			      (point-to-line x1 y1 x2 y2)))
	(t            too-big)))

(defun interpolates (x l)
  (let* ((one  (pop l))
	 (two  (first l)))
    (or (if (null l) (rest one))
	(interpolate  x (first one) (rest one) 
		        (first two) (rest two))
	(interpolates x l))))

(deftest test-line ()
  (let ((line (point-to-line 0 10 1 0))) 
    (check
      (samep line
        "#S(LINE :X1 0 :Y1 10 :X2 1 :Y2 0 :M -10 :B 10 :VERTICALP  NIL)")
      (= 5 (line-y 0.5 line))
      (= 25 (interpolates 25 '((0 . 0) (5 . 5) (30 . 30)))))))     

