(defun weather-no-train ()
  (data
   :name     'weather-no-train
   :columns  '(forecast $temp $humidty windy play)
   :egs     '((sunny 85 85 FALSE no)
              (sunny 80 90 TRUE no)
              (rainy 65 70 TRUE no)
              (sunny 72 95 FALSE no)
              (rainy 71 91 TRUE no))))
