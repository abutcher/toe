(defun ttest-from-lists (one two &optional (conf 95)) 
  (let ((as 0) (asq 0) (an 0)(bs 0) (bsq 0) (bn 0))
    (dolist (a one) 
      (incf an) (incf as a) (incf asq (* a a)))
    (dolist (b two) 
      (incf bn) (incf bs b) (incf bsq (* b b)))
    (ttest as asq an bs bsq bn :conf conf)))
    
(defun ttest (as asq an bs bsq bn &key (conf 95))
  (labels 
      ((less ()  (< (/ as an) (/ bs bn)))
       (same ()  (let* ((tcrit    (tcritical (+ an bn -2) conf))
			(ssa      (- asq (/ (* as as) an)))
			(ssb      (- bsq (/ (* bs bs) bn)))
			(pooled   (/ (+ ssa ssb) (+ bn  an -2)))
			(sxasb    (sqrt (* pooled (+ (/ 1 an) 
                                                     (/ 1 bn)))))
			(tvalue   (abs (/ (- (/ bs bn) (/ as an)) 
                                          sxasb))))
		   ;(o tcrit tvalue)
		   (> tcrit tvalue))))
    (cond ((same)  0)    ; H0  : mean of a same as mean of b
	  ((less) -1)    ; H1a : mean of a <       mean of b 
	  (t       1)))) ; H1b : mean of a >       mean of b

(let ((ttable '((95 . ((   1  . 12.70   )
		       (   3  .  3.1820 )
		       (   5  .  2.5710 )
		       (  10  .  2.2280 )
		       (  20  .  2.0860 )
		       (  80  .  1.99   )
		       ( 320  .  1.97   )))
		(99 . ((   1  . 63.6570 )
		       (   3  .  5.8410 )
		       (   5  .  4.0320 )
		       (  10  .  3.1690 )
		       (  20  .  2.8450 )
		       (  80  .  2.64   )
		       ( 320  .  2.58   ))))))
 (defun tcritical (n conf)
   (interpolates n (geta conf ttable))))

;www.cas.buffalo.edu/classes/psy/segal/2072001/ttests/t-tests1.html
(defun ttest-demo (&optional (fudge 1))  
  (let ((one '(105 112  96 124 103  92 97  108 105 110))
	(two '( 98 108 114 106 117 118 126 116 122 108)))
    (setf one (mapcar #'(lambda (x) (* x fudge)) one))
    (ttest-from-lists one two)))

(deftest test-ttest ()
  (check
    (= 0 (ttest-demo))
    (= 0 (ttest-demo 1.1))
    (= 1 (ttest-demo 1.2))))