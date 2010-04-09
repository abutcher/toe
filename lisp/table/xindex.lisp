
;;;; xindex runs over the data. populates the "counts" of each column header.
;; genrate the indexes
(defun xindex (tbl)
  (unless (table-indexed tbl)
    (setf (table-indexed tbl) t)
    (dolist (row (table-all tbl) tbl) ; for al rows do ...
      (xindex1 (eg-class row) 
	       (eg-features row) 
	       (table-columns tbl))))
  tbl)

(defun xindex1 (class datums columns) ; for all datum in a row do ...
  (mapc #'(lambda (column datum) 
	    (unless (ignorep datum)
	      (xindex-datum column class datum)))
	columns
	datums))

(defmethod xindex-datum ((column discrete) class  datum)
  (let* ((key `(,class ,datum))
	 (hash (header-f column)))
    (incf (gethash key hash 0))))

(defmethod xindex-datum ((column numeric) class  datum)
  (let* ((key      class)
	 (hash     (header-f column))
	 (counter  (gethash  key hash (make-normal))))
    (setf (gethash key hash) counter) ; make sure the hash has the counter
    (add counter datum)))

(defun make-data2 ()
  (data
   :name     'weather
   :columns  '(forecast temp humidty windy play)
   :egs     '((sunny    hot  high   FALSE no) 
              (sunny    hot  high   TRUE  no)
              (rainy    cool normal TRUE  no)
              (rainy    mild high   TRUE   no)
              (sunny    mild high   FALSE no)
              (overcast cool normal TRUE  yes)
              (overcast hot  high   FALSE yes)
              (rainy    mild high   FALSE yes)
              (rainy    cool normal FALSE yes)
              (sunny    cool normal FALSE yes)
              (rainy    mild normal FALSE yes)
              (sunny    mild normal TRUE  yes)
              (overcast mild high   TRUE  yes)
              (overcast hot  normal FALSE yes)
)))

(defun make-data3 ()
  (data
   :name   'weather
   :columns '(forecast temp humidty $wind play)
   :egs    '((sunny    hot  high   20  no) 
	     (sunny    hot  high   10 no) 
	     (sunny    hot  high   30  no) 
             (sunny    hot  high   20.2  yes)
             (sunny    hot  high   20.1  yes)
             (sunny    hot  high   20.7  yes)
             )))

(deftest test-xindex2 ()
  (check 
    (samep
     (with-output-to-string (str)
       (dolist (col (table-columns (xindex (make-data2))))
	 (format str "~%~a~%" (header-name col))
	 (showh (header-f col) :stream str)))
     "FORECAST
	    (NO RAINY) = 2
	    (NO SUNNY) = 3
	    (YES OVERCAST) = 4
	    (YES RAINY) = 3
	    (YES SUNNY) = 2
	    
      TEMP
	    (NO COOL) = 1
	    (NO HOT) = 2
	    (NO MILD) = 2
	    (YES COOL) = 3
	    (YES HOT) = 2
	    (YES MILD) = 4
	    
      HUMIDTY
	    (NO HIGH) = 4
	    (NO NORMAL) = 1
	    (YES HIGH) = 3
	    (YES NORMAL) = 6
	    
      WINDY
	    (NO FALSE) = 2
	    (NO TRUE) = 3
	    (YES FALSE) = 6
	    (YES TRUE) = 3
	    
      PLAY
	    (NO NO) = 5
	    (YES YES) = 9
	    ")))
    
(deftest test-xindex3 ()
  (check 
    (samep
     (with-output-to-string (str)
       (dolist (col (table-columns (xindex (make-data3))))
	 (format str "~%~a~%" (header-name col))
	 (showh (header-f col) :stream str)))
     "FORECAST
      (NO SUNNY) = 3
      (YES SUNNY) = 3
      
      TEMP
      (NO HOT) = 3
      (YES HOT) = 3
      
      HUMIDTY
      (NO HIGH) = 3
      (YES HIGH) = 3
      
      $WIND
      NO = #S(NORMAL :MAX 30 :MIN 10 :N 3 :SUM 60 :SUMSQ 1400)
      YES = #S(NORMAL :MAX 20.7 :MIN 20.1 :N 3 :SUM 61.000004 :SUMSQ 1240.54)
      
      PLAY
      (NO NO) = 3
      (YES YES) = 3
     ")))

(defun make-some-weather-data ()
  (data
   :name   'weather
   :columns '(forecast $temp $humidty wind play)
   :egs    '((sunny 85 85 FALSE no)
	     (sunny 80 90 TRUE no)
	     (overcast 83 86 FALSE yes)
	     (rainy 70 96 FALSE yes)
	     (rainy 68 80 FALSE yes)
	     (rainy 65 70 TRUE no)
	     (overcast 64 65 TRUE yes)
	     (sunny 72 95 FALSE no)
	     (sunny 69 70 FALSE yes)
	     (rainy 75 80 FALSE yes)
	     (sunny 75 70 TRUE yes)
	     (overcast 72 90 TRUE yes)
	     (overcast 81 75 FALSE yes)
	     (rainy 71 91 TRUE no))))

(deftest test-some-counts ()
  (check
    (samep 
     (with-output-to-string (str)
       (dolist (col (table-columns (xindex (make-some-weather-data))))
	 (format str "~%~a~%" (header-name col))
	 (showh (header-f col) :indent 10 :stream str)))
    "
    FORECAST
          (NO RAINY) = 2
          (NO SUNNY) = 3
          (YES OVERCAST) = 4
          (YES RAINY) = 3
          (YES SUNNY) = 2

    $TEMP
          NO = #S(NORMAL :MAX 85 :MIN 65 :N 5 :SUM 373 :SUMSQ 28075)
          YES = #S(NORMAL :MAX 83 :MIN 64 :N 9 :SUM 657 :SUMSQ 48265)

    $HUMIDTY
          NO = #S(NORMAL :MAX 95 :MIN 70 :N 5 :SUM 431 :SUMSQ 37531)
          YES = #S(NORMAL :MAX 96 :MIN 65 :N 9 :SUM 712 :SUMSQ 57162)

    WIND
          (NO FALSE) = 2
          (NO TRUE) = 3
          (YES FALSE) = 6
          (YES TRUE) = 3

    PLAY
          (NO NO) = 5
          (YES YES) = 9")))
    
;; e.g. query the structures

(defun f (tbl &optional class index range)
  (cond ((null class) 	 ; return number of instances 
	 (length (table-all tbl)))
	((null index)	 ; return number of a certain class 
	 (f1 (nth (table-class tbl) (table-columns tbl))
	     class
	     class))
	(t      	 ; return frequency of a range in a class
	 (f1 (nth index (table-columns tbl))
	     class
	     range))))

(defmethod f1 ((column discrete) class range)
  (gethash `(,class ,range) (header-f  column) 0))


(deftest test-f ()
  (let ((tbl (xindex (make-data3))))
    (check
      (samep 6 (f tbl))
      (samep 3 (f tbl 'yes))
      (samep 3 (f tbl 'yes 1 'hot))
      )))
