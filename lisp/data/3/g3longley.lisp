;% Data from StatLib (ftp stat.cmu.edu/datasets)
;%
;% The infamous Longley data, "An appraisal of least-squares programs from
;%  the point of view of the user", JASA, 62(1967) p819-841.
;%
;% Variables are: Number of people employed   (usually the y variable)
;%                GNP implicit price deflator
;%                GNP
;%                Unemployed
;%                Armed forces
;%                Non-institutionalized population >=14 years of age
;%                Year
;%
;% Employment is being treated as the class
;% attribute.
(defun g3longley ()
  (data
    :name 'g3longley
    :columns '($deflator $GNP $unemployed $armed_forces $population $year $employed)
    :egs
    '(
      (83 234289 2356 1590 107608 1947 60323)
      (88.5 259426 2325 1456 108632 1948 61122)
      (88.2 258054 3682 1616 109773 1949 60171)
      (89.5 284599 3351 1650 110929 1950 61187)
      (96.2 328975 2099 3099 112075 1951 63221)
      (98.1 346999 1932 3594 113270 1952 63639)
      (99 365385 1870 3547 115094 1953 64989)
      (100 363112 3578 3350 116219 1954 63761)
      (101.2 397469 2904 3048 117388 1955 66019)
      (104.6 419180 2822 2857 118734 1956 67857)
      (108.4 442769 2936 2798 120445 1957 68169)
      (110.8 444546 4681 2637 121950 1958 66513)
      (112.6 482704 3813 2552 123366 1959 68655)
      (114.2 502601 3931 2514 125368 1960 69564)
      (115.7 518173 4806 2572 127852 1961 69331)
      (116.9 554894 4007 2827 130081 1962 70551)
      )))
