(defun randomizer (tbl)
  "Randomly re-orders rows in a table"
  (data
   :name (table-name tbl)
   :columns (car (transpose (super-transform tbl)))
   :klass (table-class tbl)
   :egs (shuffle (mapcar #'eg-features (egs tbl)))
   ))

(deftest test-random ()
  (randomizer (weather-numerics)))