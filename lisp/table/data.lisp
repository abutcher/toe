;;;; the data function returns a table containing some egs


(defun data (&key name columns egs  (klass -1))
  (let* (tmp-egs
	 (tbl
          (make-table
           :name name
           :columns (columns-new
                     columns
                     (class-index klass (length columns))))))
    (setf (table-class tbl) 
	  (class-index klass (table-width tbl)))
    (dolist (eg egs)
      (if (setf eg (datums eg tbl))
	  (push eg tmp-egs)))
    tbl))

(defun datums (one tbl)
  (let ((oops (table-cautions tbl)))
    (when
	(ok (= (table-width tbl) (length one))
	    oops "~a wrong size" one)
      (mapc #'(lambda(column datum)
		(datum column datum oops))
	    (table-columns tbl) 
	    one)
      (push (make-eg :class (isa one tbl) :features one)
	    (table-all tbl)))))

(defmethod datum ((column discrete) datum oops)
  "things to do when reading a descrete datum"
  (declare (ignore  oops))
  (unless (member datum (discrete-uniques column))
    (push datum (discrete-uniques column)))
  t)

(defmethod datum ((column numeric) datum oops)
  "things to do when reading a numeric datum"
  (ok (numberp datum) oops"~a is not a number" datum)
  t)

(defun class-index (klass width)
  (if (< klass 0) (+ klass width) klass))

(defun make-data1 ()
  (data
   :name   'weather
   :columns '(forecast temp humidty $windy play)
   :egs    '((sunny    hot  high   FALSE no) 
             (sunny    hot  high   TRUE  yes)
             (sunny    hot  high         yes)
             )))

(deftest test-table ()
  (check 
    (samep 
     (make-data1)
  "#S(TABLE
   :NAME WEATHER
   :COLUMNS (#S(DISCRETE
                :NAME FORECAST
                :CLASSP NIL
                :IGNOREP NIL
                :F #<HT 0>
                :UNIQUES (SUNNY))
             #S(DISCRETE
                :NAME TEMP
                :CLASSP NIL
                :IGNOREP NIL
                :F #<HT 0>
                :UNIQUES (HOT))
             #S(DISCRETE
                :NAME HUMIDTY
                :CLASSP NIL
                :IGNOREP NIL
                :F #<HT 0>
                :UNIQUES (HIGH))
             #S(NUMERIC :NAME $WINDY :CLASSP NIL :IGNOREP NIL :F #<HT 0>)
             #S(DISCRETE
                :NAME PLAY
                :CLASSP T
                :IGNOREP NIL
                :F #<HT 0>
                :UNIQUES (YES NO)))
   :CLASS 4
   :CAUTIONS #(CAUTION :ALL ((SUNNY HOT HIGH YES) wrong size
                             TRUE is not a number
                             FALSE is not a number) :PATIENCE 17)
   :ALL (#S(EG :FEATURES (SUNNY HOT HIGH TRUE YES) :CLASS YES)
         #S(EG :FEATURES (SUNNY HOT HIGH FALSE NO) :CLASS NO))
   :INDEXED NIL)
"
)))

