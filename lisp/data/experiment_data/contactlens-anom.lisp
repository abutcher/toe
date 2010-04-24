(defun contact-lens-anom ()
  (data
   :name 'contact-lens-anom
   :columns '(age perscription astigmatism tear-production lens)
   :egs
   '(
     (young          myope        yes normal hard)
     (young          hypermetrope yes normal hard)    
     (presbyopic     myope        yes normal hard)
     (pre-presbyopic myope        yes normal hard)
     )))
