(defun bore (tbl colnum &key (best 0.2) (lib t))
  (if (eql lib t)
      (utility tbl (score-bore tbl colnum :best best :op #'>))
      (utility tbl (score-bore tbl colnum :best best :op #'<))))

(deftest test-bore ()
  "Testing bore"
  (format t "#####################(bore (weather-numerics) 1 :lib nil)#####################~%")
  (print (bore (weather-numerics) 1 :lib nil))
  (format t "~%#####################(bore (weather-numerics) 1)#####################~%")
  (print (bore (weather-numerics) 1))
  (format t "~%#####################(bore (weather-numerics) 0)#####################~%")
  (print (bore (weather-numerics) 0))
  )
