(defun weather-yes-normal-test ()
  (data
   :name     'weather-yes-normal-test
   :columns  '(forecast $temp $humidty windy play)
   :egs     '((overcast 83 86 FALSE yes)
              (rainy 70 96 FALSE yes)
              (rainy 68 80 FALSE yes)              
              (overcast 64 65 TRUE yes)              
              (sunny 69 70 FALSE yes)
              (rainy 75 80 FALSE yes)
              (sunny 75 70 TRUE yes)
              (overcast 72 90 TRUE yes)
              (overcast 81 75 FALSE yes)
              )))
