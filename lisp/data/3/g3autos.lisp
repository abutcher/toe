;% 1. Title: 1985 Auto Imports Database
;%
;% 2. Source Information:
;%    -- Creator/Donor: Jeffrey C. Schlimmer (Jeffrey.Schlimmer@a.gp.cs.cmu.edu)
;%    -- Date: 19 May 1987
;%    -- Sources:
;%      1) 1985 Model Import Car and Truck Specifications, 1985 Ward's
;%         Automotive Yearbook.
;%      2) Personal Auto Manuals, Insurance Services Office, 160 Water
;%         Street, New York, NY 10038
;%      3) Insurance Collision Report, Insurance Institute for Highway
;%         Safety, Watergate 600, Washington, DC 20037
;%
;% 3. Past Usage:
;%    -- Kibler,~D., Aha,~D.~W., \& Albert,~M. (1989).  Instance-based prediction
;%       of real-valued attributes.  {\it Computational Intelligence}, {\it 5},
;%       51--57.
;% 	 -- Predicted price of car using all numeric and Boolean attributes
;% 	 -- Method: an instance-based learning (IBL) algorithm derived from a
;% 	    localized k-nearest neighbor algorithm.  Compared with a
;% 	    linear regression prediction...so all instances
;% 	    with missing attribute values were discarded.  This resulted with
;% 	    a training set of 159 instances, which was also used as a test
;% 	    set (minus the actual instance during testing).
;% 	 -- Results: Percent Average Deviation Error of Prediction from Actual
;% 	    -- 11.84% for the IBL algorithm
;% 	    -- 14.12% for the resulting linear regression equation
;%
;% 4. Relevant Information:
;%    -- Description
;%       This data set consists of three types of entities: (a) the
;%       specification of an auto in terms of various characteristics, (b)
;%       its assigned insurance risk rating, (c) its normalized losses in use
;%       as compared to other cars.  The second rating corresponds to the
;%       degree to which the auto is more risky than its price indicates.
;%       Cars are initially assigned a risk factor symbol associated with its
;%       price.   Then, if it is more risky (or less), this symbol is
;%       adjusted by moving it up (or down) the scale.  Actuarians call this
;%       process "symboling".  A value of +3 indicates that the auto is
;%       risky, -3 that it is probably pretty safe.
;%
;%       The third factor is the relative average loss payment per insured
;%       vehicle year.  This value is normalized for all autos within a
;%       particular size classification (two-door small, station wagons,
;%       sports/speciality, etc...), and represents the average loss per car
;%       per year.
;%
;%    -- Note: Several of the attributes in the database could be used as a
;%             "class" attribute.
;%
;% 5. Number of Instances: 205
;%
;% 6. Number of Attributes: 26 total
;%    -- 15 continuous
;%    -- 1 integer
;%    -- 10 nominal
;%
;% 7. Attribute Information:
;%      Attribute:                Attribute Range:
;%      ------------------        -----------------------------------------------
;%   1. symboling:                -3, -2, -1, 0, 1, 2, 3.
;%   1. normalized-losses:        continuous from 65 to 256.
;%   3. make:                     alfa-romero, audi, bmw, chevrolet, dodge, honda,
;%                                isuzu, jaguar, mazda, mercedes-benz, mercury,
;%                                mitsubishi, nissan, peugot, plymouth, porsche,
;%                                renault, saab, subaru, toyota, volkswagen, volvo
;%   4. fuel-type:                diesel, gas.
;%   5. aspiration:               std, turbo.
;%   6. num-of-doors:             four, two.
;%   7. body-style:               hardtop, wagon, sedan, hatchback, convertible.
;%   8. drive-wheels:             4wd, fwd, rwd.
;%   9. engine-location:          front, rear.
;%  10. wheel-base:               continuous from 86.6 120.9.
;%  11. length:                   continuous from 141.1 to 208.1.
;%  12. width:                    continuous from 60.3 to 72.3.
;%  13. height:                   continuous from 47.8 to 59.8.
;%  14. curb-weight:              continuous from 1488 to 4066.
;%  15. engine-type:              dohc, dohcv, l, ohc, ohcf, ohcv, rotor.
;%  16. num-of-cylinders:         eight, five, four, six, three, twelve, two.
;%  17. engine-size:              continuous from 61 to 326.
;%  18. fuel-system:              1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi.
;%  19. bore:                     continuous from 2.54 to 3.94.
;%  20. stroke:                   continuous from 2.07 to 4.17.
;%  21. compression-ratio:        continuous from 7 to 23.
;%  22. horsepower:               continuous from 48 to 288.
;%  23. peak-rpm:                 continuous from 4150 to 6600.
;%  24. city-mpg:                 continuous from 13 to 49.
;%  25. highway-mpg:              continuous from 16 to 54.
;%  26. price:                    continuous from 5118 to 45400.
;%
;% 8. Missing Attribute Values: (denoted by "?")
;%    Attribute #:   Number of instances missing a value:
;%    2.             41
;%    6.             2
;%    19.            4
;%    20.            4
;%    22.            2
;%    23.            2
;%    26.            4
;%
;%
;%
;%
(defun g3autos ()
  (data
    :name 'g3autos
    :columns '($normalized-losses make fuel-type aspiration num-of-doors body-style drive-wheels engine-location $wheel-base $length $width $height $curb-weight engine-type num-of-cylinders $engine-size fuel-system $bore $stroke $compression-ratio $horsepower $peak-rpm $city-mpg $highway-mpg $price symboling)
    :egs
    '(
      (? alfa-romero gas std two convertible rwd front 88.6 168.8 64.1 48.8 2548 dohc four 130 mpfi 3.47 2.68 9 111 5000 21 27 13495 3)
      (? alfa-romero gas std two convertible rwd front 88.6 168.8 64.1 48.8 2548 dohc four 130 mpfi 3.47 2.68 9 111 5000 21 27 16500 3)
      (? alfa-romero gas std two hatchback rwd front 94.5 171.2 65.5 52.4 2823 ohcv six 152 mpfi 2.68 3.47 9 154 5000 19 26 16500 1)
      (164 audi gas std four sedan fwd front 99.8 176.6 66.2 54.3 2337 ohc four 109 mpfi 3.19 3.4 10 102 5500 24 30 13950 2)
      (164 audi gas std four sedan 4wd front 99.4 176.6 66.4 54.3 2824 ohc five 136 mpfi 3.19 3.4 8 115 5500 18 22 17450 2)
      (? audi gas std two sedan fwd front 99.8 177.3 66.3 53.1 2507 ohc five 136 mpfi 3.19 3.4 8.5 110 5500 19 25 15250 2)
      (158 audi gas std four sedan fwd front 105.8 192.7 71.4 55.7 2844 ohc five 136 mpfi 3.19 3.4 8.5 110 5500 19 25 17710 1)
      (? audi gas std four wagon fwd front 105.8 192.7 71.4 55.7 2954 ohc five 136 mpfi 3.19 3.4 8.5 110 5500 19 25 18920 1)
      (158 audi gas turbo four sedan fwd front 105.8 192.7 71.4 55.9 3086 ohc five 131 mpfi 3.13 3.4 8.3 140 5500 17 20 23875 1)
      (? audi gas turbo two hatchback 4wd front 99.5 178.2 67.9 52 3053 ohc five 131 mpfi 3.13 3.4 7 160 5500 16 22 ? 0)
      (192 bmw gas std two sedan rwd front 101.2 176.8 64.8 54.3 2395 ohc four 108 mpfi 3.5 2.8 8.8 101 5800 23 29 16430 2)
      (192 bmw gas std four sedan rwd front 101.2 176.8 64.8 54.3 2395 ohc four 108 mpfi 3.5 2.8 8.8 101 5800 23 29 16925 0)
      (188 bmw gas std two sedan rwd front 101.2 176.8 64.8 54.3 2710 ohc six 164 mpfi 3.31 3.19 9 121 4250 21 28 20970 0)
      (188 bmw gas std four sedan rwd front 101.2 176.8 64.8 54.3 2765 ohc six 164 mpfi 3.31 3.19 9 121 4250 21 28 21105 0)
      (? bmw gas std four sedan rwd front 103.5 189 66.9 55.7 3055 ohc six 164 mpfi 3.31 3.19 9 121 4250 20 25 24565 1)
      (? bmw gas std four sedan rwd front 103.5 189 66.9 55.7 3230 ohc six 209 mpfi 3.62 3.39 8 182 5400 16 22 30760 0)
      (? bmw gas std two sedan rwd front 103.5 193.8 67.9 53.7 3380 ohc six 209 mpfi 3.62 3.39 8 182 5400 16 22 41315 0)
      (? bmw gas std four sedan rwd front 110 197 70.9 56.3 3505 ohc six 209 mpfi 3.62 3.39 8 182 5400 15 20 36880 0)
      (121 chevrolet gas std two hatchback fwd front 88.4 141.1 60.3 53.2 1488 l three 61 2bbl 2.91 3.03 9.5 48 5100 47 53 5151 2)
      (98 chevrolet gas std two hatchback fwd front 94.5 155.9 63.6 52 1874 ohc four 90 2bbl 3.03 3.11 9.6 70 5400 38 43 6295 1)
      (81 chevrolet gas std four sedan fwd front 94.5 158.8 63.6 52 1909 ohc four 90 2bbl 3.03 3.11 9.6 70 5400 38 43 6575 0)
      (118 dodge gas std two hatchback fwd front 93.7 157.3 63.8 50.8 1876 ohc four 90 2bbl 2.97 3.23 9.41 68 5500 37 41 5572 1)
      (118 dodge gas std two hatchback fwd front 93.7 157.3 63.8 50.8 1876 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 31 38 6377 1)
      (118 dodge gas turbo two hatchback fwd front 93.7 157.3 63.8 50.8 2128 ohc four 98 mpfi 3.03 3.39 7.6 102 5500 24 30 7957 1)
      (148 dodge gas std four hatchback fwd front 93.7 157.3 63.8 50.6 1967 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 31 38 6229 1)
      (148 dodge gas std four sedan fwd front 93.7 157.3 63.8 50.6 1989 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 31 38 6692 1)
      (148 dodge gas std four sedan fwd front 93.7 157.3 63.8 50.6 1989 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 31 38 7609 1)
      (148 dodge gas turbo ? sedan fwd front 93.7 157.3 63.8 50.6 2191 ohc four 98 mpfi 3.03 3.39 7.6 102 5500 24 30 8558 1)
      (110 dodge gas std four wagon fwd front 103.3 174.6 64.6 59.8 2535 ohc four 122 2bbl 3.34 3.46 8.5 88 5000 24 30 8921 -1)
      (145 dodge gas turbo two hatchback fwd front 95.9 173.2 66.3 50.2 2811 ohc four 156 mfi 3.6 3.9 7 145 5000 19 24 12964 3)
      (137 honda gas std two hatchback fwd front 86.6 144.6 63.9 50.8 1713 ohc four 92 1bbl 2.91 3.41 9.6 58 4800 49 54 6479 2)
      (137 honda gas std two hatchback fwd front 86.6 144.6 63.9 50.8 1819 ohc four 92 1bbl 2.91 3.41 9.2 76 6000 31 38 6855 2)
      (101 honda gas std two hatchback fwd front 93.7 150 64 52.6 1837 ohc four 79 1bbl 2.91 3.07 10.1 60 5500 38 42 5399 1)
      (101 honda gas std two hatchback fwd front 93.7 150 64 52.6 1940 ohc four 92 1bbl 2.91 3.41 9.2 76 6000 30 34 6529 1)
      (101 honda gas std two hatchback fwd front 93.7 150 64 52.6 1956 ohc four 92 1bbl 2.91 3.41 9.2 76 6000 30 34 7129 1)
      (110 honda gas std four sedan fwd front 96.5 163.4 64 54.5 2010 ohc four 92 1bbl 2.91 3.41 9.2 76 6000 30 34 7295 0)
      (78 honda gas std four wagon fwd front 96.5 157.1 63.9 58.3 2024 ohc four 92 1bbl 2.92 3.41 9.2 76 6000 30 34 7295 0)
      (106 honda gas std two hatchback fwd front 96.5 167.5 65.2 53.3 2236 ohc four 110 1bbl 3.15 3.58 9 86 5800 27 33 7895 0)
      (106 honda gas std two hatchback fwd front 96.5 167.5 65.2 53.3 2289 ohc four 110 1bbl 3.15 3.58 9 86 5800 27 33 9095 0)
      (85 honda gas std four sedan fwd front 96.5 175.4 65.2 54.1 2304 ohc four 110 1bbl 3.15 3.58 9 86 5800 27 33 8845 0)
      (85 honda gas std four sedan fwd front 96.5 175.4 62.5 54.1 2372 ohc four 110 1bbl 3.15 3.58 9 86 5800 27 33 10295 0)
      (85 honda gas std four sedan fwd front 96.5 175.4 65.2 54.1 2465 ohc four 110 mpfi 3.15 3.58 9 101 5800 24 28 12945 0)
      (107 honda gas std two sedan fwd front 96.5 169.1 66 51 2293 ohc four 110 2bbl 3.15 3.58 9.1 100 5500 25 31 10345 1)
      (? isuzu gas std four sedan rwd front 94.3 170.7 61.8 53.5 2337 ohc four 111 2bbl 3.31 3.23 8.5 78 4800 24 29 6785 0)
      (? isuzu gas std two sedan fwd front 94.5 155.9 63.6 52 1874 ohc four 90 2bbl 3.03 3.11 9.6 70 5400 38 43 ? 1)
      (? isuzu gas std four sedan fwd front 94.5 155.9 63.6 52 1909 ohc four 90 2bbl 3.03 3.11 9.6 70 5400 38 43 ? 0)
      (? isuzu gas std two hatchback rwd front 96 172.6 65.2 51.4 2734 ohc four 119 spfi 3.43 3.23 9.2 90 5000 24 29 11048 2)
      (145 jaguar gas std four sedan rwd front 113 199.6 69.6 52.8 4066 dohc six 258 mpfi 3.63 4.17 8.1 176 4750 15 19 32250 0)
      (? jaguar gas std four sedan rwd front 113 199.6 69.6 52.8 4066 dohc six 258 mpfi 3.63 4.17 8.1 176 4750 15 19 35550 0)
      (? jaguar gas std two sedan rwd front 102 191.7 70.6 47.8 3950 ohcv twelve 326 mpfi 3.54 2.76 11.5 262 5000 13 17 36000 0)
      (104 mazda gas std two hatchback fwd front 93.1 159.1 64.2 54.1 1890 ohc four 91 2bbl 3.03 3.15 9 68 5000 30 31 5195 1)
      (104 mazda gas std two hatchback fwd front 93.1 159.1 64.2 54.1 1900 ohc four 91 2bbl 3.03 3.15 9 68 5000 31 38 6095 1)
      (104 mazda gas std two hatchback fwd front 93.1 159.1 64.2 54.1 1905 ohc four 91 2bbl 3.03 3.15 9 68 5000 31 38 6795 1)
      (113 mazda gas std four sedan fwd front 93.1 166.8 64.2 54.1 1945 ohc four 91 2bbl 3.03 3.15 9 68 5000 31 38 6695 1)
      (113 mazda gas std four sedan fwd front 93.1 166.8 64.2 54.1 1950 ohc four 91 2bbl 3.08 3.15 9 68 5000 31 38 7395 1)
      (150 mazda gas std two hatchback rwd front 95.3 169 65.7 49.6 2380 rotor two 70 4bbl ? ? 9.4 101 6000 17 23 10945 3)
      (150 mazda gas std two hatchback rwd front 95.3 169 65.7 49.6 2380 rotor two 70 4bbl ? ? 9.4 101 6000 17 23 11845 3)
      (150 mazda gas std two hatchback rwd front 95.3 169 65.7 49.6 2385 rotor two 70 4bbl ? ? 9.4 101 6000 17 23 13645 3)
      (150 mazda gas std two hatchback rwd front 95.3 169 65.7 49.6 2500 rotor two 80 mpfi ? ? 9.4 135 6000 16 23 15645 3)
      (129 mazda gas std two hatchback fwd front 98.8 177.8 66.5 53.7 2385 ohc four 122 2bbl 3.39 3.39 8.6 84 4800 26 32 8845 1)
      (115 mazda gas std four sedan fwd front 98.8 177.8 66.5 55.5 2410 ohc four 122 2bbl 3.39 3.39 8.6 84 4800 26 32 8495 0)
      (129 mazda gas std two hatchback fwd front 98.8 177.8 66.5 53.7 2385 ohc four 122 2bbl 3.39 3.39 8.6 84 4800 26 32 10595 1)
      (115 mazda gas std four sedan fwd front 98.8 177.8 66.5 55.5 2410 ohc four 122 2bbl 3.39 3.39 8.6 84 4800 26 32 10245 0)
      (? mazda diesel std ? sedan fwd front 98.8 177.8 66.5 55.5 2443 ohc four 122 idi 3.39 3.39 22.7 64 4650 36 42 10795 0)
      (115 mazda gas std four hatchback fwd front 98.8 177.8 66.5 55.5 2425 ohc four 122 2bbl 3.39 3.39 8.6 84 4800 26 32 11245 0)
      (118 mazda gas std four sedan rwd front 104.9 175 66.1 54.4 2670 ohc four 140 mpfi 3.76 3.16 8 120 5000 19 27 18280 0)
      (? mazda diesel std four sedan rwd front 104.9 175 66.1 54.4 2700 ohc four 134 idi 3.43 3.64 22 72 4200 31 39 18344 0)
      (93 mercedes-benz diesel turbo four sedan rwd front 110 190.9 70.3 56.5 3515 ohc five 183 idi 3.58 3.64 21.5 123 4350 22 25 25552 -1)
      (93 mercedes-benz diesel turbo four wagon rwd front 110 190.9 70.3 58.7 3750 ohc five 183 idi 3.58 3.64 21.5 123 4350 22 25 28248 -1)
      (93 mercedes-benz diesel turbo two hardtop rwd front 106.7 187.5 70.3 54.9 3495 ohc five 183 idi 3.58 3.64 21.5 123 4350 22 25 28176 0)
      (93 mercedes-benz diesel turbo four sedan rwd front 115.6 202.6 71.7 56.3 3770 ohc five 183 idi 3.58 3.64 21.5 123 4350 22 25 31600 -1)
      (? mercedes-benz gas std four sedan rwd front 115.6 202.6 71.7 56.5 3740 ohcv eight 234 mpfi 3.46 3.1 8.3 155 4750 16 18 34184 -1)
      (142 mercedes-benz gas std two convertible rwd front 96.6 180.3 70.5 50.8 3685 ohcv eight 234 mpfi 3.46 3.1 8.3 155 4750 16 18 35056 3)
      (? mercedes-benz gas std four sedan rwd front 120.9 208.1 71.7 56.7 3900 ohcv eight 308 mpfi 3.8 3.35 8 184 4500 14 16 40960 0)
      (? mercedes-benz gas std two hardtop rwd front 112 199.2 72 55.4 3715 ohcv eight 304 mpfi 3.8 3.35 8 184 4500 14 16 45400 1)
      (? mercury gas turbo two hatchback rwd front 102.7 178.4 68 54.8 2910 ohc four 140 mpfi 3.78 3.12 8 175 5000 19 24 16503 1)
      (161 mitsubishi gas std two hatchback fwd front 93.7 157.3 64.4 50.8 1918 ohc four 92 2bbl 2.97 3.23 9.4 68 5500 37 41 5389 2)
      (161 mitsubishi gas std two hatchback fwd front 93.7 157.3 64.4 50.8 1944 ohc four 92 2bbl 2.97 3.23 9.4 68 5500 31 38 6189 2)
      (161 mitsubishi gas std two hatchback fwd front 93.7 157.3 64.4 50.8 2004 ohc four 92 2bbl 2.97 3.23 9.4 68 5500 31 38 6669 2)
      (161 mitsubishi gas turbo two hatchback fwd front 93 157.3 63.8 50.8 2145 ohc four 98 spdi 3.03 3.39 7.6 102 5500 24 30 7689 1)
      (153 mitsubishi gas turbo two hatchback fwd front 96.3 173 65.4 49.4 2370 ohc four 110 spdi 3.17 3.46 7.5 116 5500 23 30 9959 3)
      (153 mitsubishi gas std two hatchback fwd front 96.3 173 65.4 49.4 2328 ohc four 122 2bbl 3.35 3.46 8.5 88 5000 25 32 8499 3)
      (? mitsubishi gas turbo two hatchback fwd front 95.9 173.2 66.3 50.2 2833 ohc four 156 spdi 3.58 3.86 7 145 5000 19 24 12629 3)
      (? mitsubishi gas turbo two hatchback fwd front 95.9 173.2 66.3 50.2 2921 ohc four 156 spdi 3.59 3.86 7 145 5000 19 24 14869 3)
      (? mitsubishi gas turbo two hatchback fwd front 95.9 173.2 66.3 50.2 2926 ohc four 156 spdi 3.59 3.86 7 145 5000 19 24 14489 3)
      (125 mitsubishi gas std four sedan fwd front 96.3 172.4 65.4 51.6 2365 ohc four 122 2bbl 3.35 3.46 8.5 88 5000 25 32 6989 1)
      (125 mitsubishi gas std four sedan fwd front 96.3 172.4 65.4 51.6 2405 ohc four 122 2bbl 3.35 3.46 8.5 88 5000 25 32 8189 1)
      (125 mitsubishi gas turbo four sedan fwd front 96.3 172.4 65.4 51.6 2403 ohc four 110 spdi 3.17 3.46 7.5 116 5500 23 30 9279 1)
      (137 mitsubishi gas std four sedan fwd front 96.3 172.4 65.4 51.6 2403 ohc four 110 spdi 3.17 3.46 7.5 116 5500 23 30 9279 -1)
      (128 nissan gas std two sedan fwd front 94.5 165.3 63.8 54.5 1889 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 5499 1)
      (128 nissan diesel std two sedan fwd front 94.5 165.3 63.8 54.5 2017 ohc four 103 idi 2.99 3.47 21.9 55 4800 45 50 7099 1)
      (128 nissan gas std two sedan fwd front 94.5 165.3 63.8 54.5 1918 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 6649 1)
      (122 nissan gas std four sedan fwd front 94.5 165.3 63.8 54.5 1938 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 6849 1)
      (103 nissan gas std four wagon fwd front 94.5 170.2 63.8 53.5 2024 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 7349 1)
      (128 nissan gas std two sedan fwd front 94.5 165.3 63.8 54.5 1951 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 7299 1)
      (128 nissan gas std two hatchback fwd front 94.5 165.6 63.8 53.3 2028 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 7799 1)
      (122 nissan gas std four sedan fwd front 94.5 165.3 63.8 54.5 1971 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 7499 1)
      (103 nissan gas std four wagon fwd front 94.5 170.2 63.8 53.5 2037 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 7999 1)
      (168 nissan gas std two hardtop fwd front 95.1 162.4 63.8 53.3 2008 ohc four 97 2bbl 3.15 3.29 9.4 69 5200 31 37 8249 2)
      (106 nissan gas std four hatchback fwd front 97.2 173.4 65.2 54.7 2324 ohc four 120 2bbl 3.33 3.47 8.5 97 5200 27 34 8949 0)
      (106 nissan gas std four sedan fwd front 97.2 173.4 65.2 54.7 2302 ohc four 120 2bbl 3.33 3.47 8.5 97 5200 27 34 9549 0)
      (128 nissan gas std four sedan fwd front 100.4 181.7 66.5 55.1 3095 ohcv six 181 mpfi 3.43 3.27 9 152 5200 17 22 13499 0)
      (108 nissan gas std four wagon fwd front 100.4 184.6 66.5 56.1 3296 ohcv six 181 mpfi 3.43 3.27 9 152 5200 17 22 14399 0)
      (108 nissan gas std four sedan fwd front 100.4 184.6 66.5 55.1 3060 ohcv six 181 mpfi 3.43 3.27 9 152 5200 19 25 13499 0)
      (194 nissan gas std two hatchback rwd front 91.3 170.7 67.9 49.7 3071 ohcv six 181 mpfi 3.43 3.27 9 160 5200 19 25 17199 3)
      (194 nissan gas turbo two hatchback rwd front 91.3 170.7 67.9 49.7 3139 ohcv six 181 mpfi 3.43 3.27 7.8 200 5200 17 23 19699 3)
      (231 nissan gas std two hatchback rwd front 99.2 178.5 67.9 49.7 3139 ohcv six 181 mpfi 3.43 3.27 9 160 5200 19 25 18399 1)
      (161 peugot gas std four sedan rwd front 107.9 186.7 68.4 56.7 3020 l four 120 mpfi 3.46 3.19 8.4 97 5000 19 24 11900 0)
      (161 peugot diesel turbo four sedan rwd front 107.9 186.7 68.4 56.7 3197 l four 152 idi 3.7 3.52 21 95 4150 28 33 13200 0)
      (? peugot gas std four wagon rwd front 114.2 198.9 68.4 58.7 3230 l four 120 mpfi 3.46 3.19 8.4 97 5000 19 24 12440 0)
      (? peugot diesel turbo four wagon rwd front 114.2 198.9 68.4 58.7 3430 l four 152 idi 3.7 3.52 21 95 4150 25 25 13860 0)
      (161 peugot gas std four sedan rwd front 107.9 186.7 68.4 56.7 3075 l four 120 mpfi 3.46 2.19 8.4 95 5000 19 24 15580 0)
      (161 peugot diesel turbo four sedan rwd front 107.9 186.7 68.4 56.7 3252 l four 152 idi 3.7 3.52 21 95 4150 28 33 16900 0)
      (? peugot gas std four wagon rwd front 114.2 198.9 68.4 56.7 3285 l four 120 mpfi 3.46 2.19 8.4 95 5000 19 24 16695 0)
      (? peugot diesel turbo four wagon rwd front 114.2 198.9 68.4 58.7 3485 l four 152 idi 3.7 3.52 21 95 4150 25 25 17075 0)
      (161 peugot gas std four sedan rwd front 107.9 186.7 68.4 56.7 3075 l four 120 mpfi 3.46 3.19 8.4 97 5000 19 24 16630 0)
      (161 peugot diesel turbo four sedan rwd front 107.9 186.7 68.4 56.7 3252 l four 152 idi 3.7 3.52 21 95 4150 28 33 17950 0)
      (161 peugot gas turbo four sedan rwd front 108 186.7 68.3 56 3130 l four 134 mpfi 3.61 3.21 7 142 5600 18 24 18150 0)
      (119 plymouth gas std two hatchback fwd front 93.7 157.3 63.8 50.8 1918 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 37 41 5572 1)
      (119 plymouth gas turbo two hatchback fwd front 93.7 157.3 63.8 50.8 2128 ohc four 98 spdi 3.03 3.39 7.6 102 5500 24 30 7957 1)
      (154 plymouth gas std four hatchback fwd front 93.7 157.3 63.8 50.6 1967 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 31 38 6229 1)
      (154 plymouth gas std four sedan fwd front 93.7 167.3 63.8 50.8 1989 ohc four 90 2bbl 2.97 3.23 9.4 68 5500 31 38 6692 1)
      (154 plymouth gas std four sedan fwd front 93.7 167.3 63.8 50.8 2191 ohc four 98 2bbl 2.97 3.23 9.4 68 5500 31 38 7609 1)
      (74 plymouth gas std four wagon fwd front 103.3 174.6 64.6 59.8 2535 ohc four 122 2bbl 3.35 3.46 8.5 88 5000 24 30 8921 -1)
      (? plymouth gas turbo two hatchback rwd front 95.9 173.2 66.3 50.2 2818 ohc four 156 spdi 3.59 3.86 7 145 5000 19 24 12764 3)
      (186 porsche gas std two hatchback rwd front 94.5 168.9 68.3 50.2 2778 ohc four 151 mpfi 3.94 3.11 9.5 143 5500 19 27 22018 3)
      (? porsche gas std two hardtop rwd rear 89.5 168.9 65 51.6 2756 ohcf six 194 mpfi 3.74 2.9 9.5 207 5900 17 25 32528 3)
      (? porsche gas std two hardtop rwd rear 89.5 168.9 65 51.6 2756 ohcf six 194 mpfi 3.74 2.9 9.5 207 5900 17 25 34028 3)
      (? porsche gas std two convertible rwd rear 89.5 168.9 65 51.6 2800 ohcf six 194 mpfi 3.74 2.9 9.5 207 5900 17 25 37028 3)
      (? porsche gas std two hatchback rwd front 98.4 175.7 72.3 50.5 3366 dohcv eight 203 mpfi 3.94 3.11 10 288 5750 17 28 ? 1)
      (? renault gas std four wagon fwd front 96.1 181.5 66.5 55.2 2579 ohc four 132 mpfi 3.46 3.9 8.7 ? ? 23 31 9295 0)
      (? renault gas std two hatchback fwd front 96.1 176.8 66.6 50.5 2460 ohc four 132 mpfi 3.46 3.9 8.7 ? ? 23 31 9895 2)
      (150 saab gas std two hatchback fwd front 99.1 186.6 66.5 56.1 2658 ohc four 121 mpfi 3.54 3.07 9.31 110 5250 21 28 11850 3)
      (104 saab gas std four sedan fwd front 99.1 186.6 66.5 56.1 2695 ohc four 121 mpfi 3.54 3.07 9.3 110 5250 21 28 12170 2)
      (150 saab gas std two hatchback fwd front 99.1 186.6 66.5 56.1 2707 ohc four 121 mpfi 2.54 2.07 9.3 110 5250 21 28 15040 3)
      (104 saab gas std four sedan fwd front 99.1 186.6 66.5 56.1 2758 ohc four 121 mpfi 3.54 3.07 9.3 110 5250 21 28 15510 2)
      (150 saab gas turbo two hatchback fwd front 99.1 186.6 66.5 56.1 2808 dohc four 121 mpfi 3.54 3.07 9 160 5500 19 26 18150 3)
      (104 saab gas turbo four sedan fwd front 99.1 186.6 66.5 56.1 2847 dohc four 121 mpfi 3.54 3.07 9 160 5500 19 26 18620 2)
      (83 subaru gas std two hatchback fwd front 93.7 156.9 63.4 53.7 2050 ohcf four 97 2bbl 3.62 2.36 9 69 4900 31 36 5118 2)
      (83 subaru gas std two hatchback fwd front 93.7 157.9 63.6 53.7 2120 ohcf four 108 2bbl 3.62 2.64 8.7 73 4400 26 31 7053 2)
      (83 subaru gas std two hatchback 4wd front 93.3 157.3 63.8 55.7 2240 ohcf four 108 2bbl 3.62 2.64 8.7 73 4400 26 31 7603 2)
      (102 subaru gas std four sedan fwd front 97.2 172 65.4 52.5 2145 ohcf four 108 2bbl 3.62 2.64 9.5 82 4800 32 37 7126 0)
      (102 subaru gas std four sedan fwd front 97.2 172 65.4 52.5 2190 ohcf four 108 2bbl 3.62 2.64 9.5 82 4400 28 33 7775 0)
      (102 subaru gas std four sedan fwd front 97.2 172 65.4 52.5 2340 ohcf four 108 mpfi 3.62 2.64 9 94 5200 26 32 9960 0)
      (102 subaru gas std four sedan 4wd front 97 172 65.4 54.3 2385 ohcf four 108 2bbl 3.62 2.64 9 82 4800 24 25 9233 0)
      (102 subaru gas turbo four sedan 4wd front 97 172 65.4 54.3 2510 ohcf four 108 mpfi 3.62 2.64 7.7 111 4800 24 29 11259 0)
      (89 subaru gas std four wagon fwd front 97 173.5 65.4 53 2290 ohcf four 108 2bbl 3.62 2.64 9 82 4800 28 32 7463 0)
      (89 subaru gas std four wagon fwd front 97 173.5 65.4 53 2455 ohcf four 108 mpfi 3.62 2.64 9 94 5200 25 31 10198 0)
      (85 subaru gas std four wagon 4wd front 96.9 173.6 65.4 54.9 2420 ohcf four 108 2bbl 3.62 2.64 9 82 4800 23 29 8013 0)
      (85 subaru gas turbo four wagon 4wd front 96.9 173.6 65.4 54.9 2650 ohcf four 108 mpfi 3.62 2.64 7.7 111 4800 23 23 11694 0)
      (87 toyota gas std two hatchback fwd front 95.7 158.7 63.6 54.5 1985 ohc four 92 2bbl 3.05 3.03 9 62 4800 35 39 5348 1)
      (87 toyota gas std two hatchback fwd front 95.7 158.7 63.6 54.5 2040 ohc four 92 2bbl 3.05 3.03 9 62 4800 31 38 6338 1)
      (74 toyota gas std four hatchback fwd front 95.7 158.7 63.6 54.5 2015 ohc four 92 2bbl 3.05 3.03 9 62 4800 31 38 6488 1)
      (77 toyota gas std four wagon fwd front 95.7 169.7 63.6 59.1 2280 ohc four 92 2bbl 3.05 3.03 9 62 4800 31 37 6918 0)
      (81 toyota gas std four wagon 4wd front 95.7 169.7 63.6 59.1 2290 ohc four 92 2bbl 3.05 3.03 9 62 4800 27 32 7898 0)
      (91 toyota gas std four wagon 4wd front 95.7 169.7 63.6 59.1 3110 ohc four 92 2bbl 3.05 3.03 9 62 4800 27 32 8778 0)
      (91 toyota gas std four sedan fwd front 95.7 166.3 64.4 53 2081 ohc four 98 2bbl 3.19 3.03 9 70 4800 30 37 6938 0)
      (91 toyota gas std four hatchback fwd front 95.7 166.3 64.4 52.8 2109 ohc four 98 2bbl 3.19 3.03 9 70 4800 30 37 7198 0)
      (91 toyota diesel std four sedan fwd front 95.7 166.3 64.4 53 2275 ohc four 110 idi 3.27 3.35 22.5 56 4500 34 36 7898 0)
      (91 toyota diesel std four hatchback fwd front 95.7 166.3 64.4 52.8 2275 ohc four 110 idi 3.27 3.35 22.5 56 4500 38 47 7788 0)
      (91 toyota gas std four sedan fwd front 95.7 166.3 64.4 53 2094 ohc four 98 2bbl 3.19 3.03 9 70 4800 38 47 7738 0)
      (91 toyota gas std four hatchback fwd front 95.7 166.3 64.4 52.8 2122 ohc four 98 2bbl 3.19 3.03 9 70 4800 28 34 8358 0)
      (91 toyota gas std four sedan fwd front 95.7 166.3 64.4 52.8 2140 ohc four 98 2bbl 3.19 3.03 9 70 4800 28 34 9258 0)
      (168 toyota gas std two sedan rwd front 94.5 168.7 64 52.6 2169 ohc four 98 2bbl 3.19 3.03 9 70 4800 29 34 8058 1)
      (168 toyota gas std two hatchback rwd front 94.5 168.7 64 52.6 2204 ohc four 98 2bbl 3.19 3.03 9 70 4800 29 34 8238 1)
      (168 toyota gas std two sedan rwd front 94.5 168.7 64 52.6 2265 dohc four 98 mpfi 3.24 3.08 9.4 112 6600 26 29 9298 1)
      (168 toyota gas std two hatchback rwd front 94.5 168.7 64 52.6 2300 dohc four 98 mpfi 3.24 3.08 9.4 112 6600 26 29 9538 1)
      (134 toyota gas std two hardtop rwd front 98.4 176.2 65.6 52 2540 ohc four 146 mpfi 3.62 3.5 9.3 116 4800 24 30 8449 2)
      (134 toyota gas std two hardtop rwd front 98.4 176.2 65.6 52 2536 ohc four 146 mpfi 3.62 3.5 9.3 116 4800 24 30 9639 2)
      (134 toyota gas std two hatchback rwd front 98.4 176.2 65.6 52 2551 ohc four 146 mpfi 3.62 3.5 9.3 116 4800 24 30 9989 2)
      (134 toyota gas std two hardtop rwd front 98.4 176.2 65.6 52 2679 ohc four 146 mpfi 3.62 3.5 9.3 116 4800 24 30 11199 2)
      (134 toyota gas std two hatchback rwd front 98.4 176.2 65.6 52 2714 ohc four 146 mpfi 3.62 3.5 9.3 116 4800 24 30 11549 2)
      (134 toyota gas std two convertible rwd front 98.4 176.2 65.6 53 2975 ohc four 146 mpfi 3.62 3.5 9.3 116 4800 24 30 17669 2)
      (65 toyota gas std four sedan fwd front 102.4 175.6 66.5 54.9 2326 ohc four 122 mpfi 3.31 3.54 8.7 92 4200 29 34 8948 -1)
      (65 toyota diesel turbo four sedan fwd front 102.4 175.6 66.5 54.9 2480 ohc four 110 idi 3.27 3.35 22.5 73 4500 30 33 10698 -1)
      (65 toyota gas std four hatchback fwd front 102.4 175.6 66.5 53.9 2414 ohc four 122 mpfi 3.31 3.54 8.7 92 4200 27 32 9988 -1)
      (65 toyota gas std four sedan fwd front 102.4 175.6 66.5 54.9 2414 ohc four 122 mpfi 3.31 3.54 8.7 92 4200 27 32 10898 -1)
      (65 toyota gas std four hatchback fwd front 102.4 175.6 66.5 53.9 2458 ohc four 122 mpfi 3.31 3.54 8.7 92 4200 27 32 11248 -1)
      (197 toyota gas std two hatchback rwd front 102.9 183.5 67.7 52 2976 dohc six 171 mpfi 3.27 3.35 9.3 161 5200 20 24 16558 3)
      (197 toyota gas std two hatchback rwd front 102.9 183.5 67.7 52 3016 dohc six 171 mpfi 3.27 3.35 9.3 161 5200 19 24 15998 3)
      (90 toyota gas std four sedan rwd front 104.5 187.8 66.5 54.1 3131 dohc six 171 mpfi 3.27 3.35 9.2 156 5200 20 24 15690 -1)
      (? toyota gas std four wagon rwd front 104.5 187.8 66.5 54.1 3151 dohc six 161 mpfi 3.27 3.35 9.2 156 5200 19 24 15750 -1)
      (122 volkswagen diesel std two sedan fwd front 97.3 171.7 65.5 55.7 2261 ohc four 97 idi 3.01 3.4 23 52 4800 37 46 7775 2)
      (122 volkswagen gas std two sedan fwd front 97.3 171.7 65.5 55.7 2209 ohc four 109 mpfi 3.19 3.4 9 85 5250 27 34 7975 2)
      (94 volkswagen diesel std four sedan fwd front 97.3 171.7 65.5 55.7 2264 ohc four 97 idi 3.01 3.4 23 52 4800 37 46 7995 2)
      (94 volkswagen gas std four sedan fwd front 97.3 171.7 65.5 55.7 2212 ohc four 109 mpfi 3.19 3.4 9 85 5250 27 34 8195 2)
      (94 volkswagen gas std four sedan fwd front 97.3 171.7 65.5 55.7 2275 ohc four 109 mpfi 3.19 3.4 9 85 5250 27 34 8495 2)
      (94 volkswagen diesel turbo four sedan fwd front 97.3 171.7 65.5 55.7 2319 ohc four 97 idi 3.01 3.4 23 68 4500 37 42 9495 2)
      (94 volkswagen gas std four sedan fwd front 97.3 171.7 65.5 55.7 2300 ohc four 109 mpfi 3.19 3.4 10 100 5500 26 32 9995 2)
      (? volkswagen gas std two convertible fwd front 94.5 159.3 64.2 55.6 2254 ohc four 109 mpfi 3.19 3.4 8.5 90 5500 24 29 11595 3)
      (256 volkswagen gas std two hatchback fwd front 94.5 165.7 64 51.4 2221 ohc four 109 mpfi 3.19 3.4 8.5 90 5500 24 29 9980 3)
      (? volkswagen gas std four sedan fwd front 100.4 180.2 66.9 55.1 2661 ohc five 136 mpfi 3.19 3.4 8.5 110 5500 19 24 13295 0)
      (? volkswagen diesel turbo four sedan fwd front 100.4 180.2 66.9 55.1 2579 ohc four 97 idi 3.01 3.4 23 68 4500 33 38 13845 0)
      (? volkswagen gas std four wagon fwd front 100.4 183.1 66.9 55.1 2563 ohc four 109 mpfi 3.19 3.4 9 88 5500 25 31 12290 0)
      (103 volvo gas std four sedan rwd front 104.3 188.8 67.2 56.2 2912 ohc four 141 mpfi 3.78 3.15 9.5 114 5400 23 28 12940 -2)
      (74 volvo gas std four wagon rwd front 104.3 188.8 67.2 57.5 3034 ohc four 141 mpfi 3.78 3.15 9.5 114 5400 23 28 13415 -1)
      (103 volvo gas std four sedan rwd front 104.3 188.8 67.2 56.2 2935 ohc four 141 mpfi 3.78 3.15 9.5 114 5400 24 28 15985 -2)
      (74 volvo gas std four wagon rwd front 104.3 188.8 67.2 57.5 3042 ohc four 141 mpfi 3.78 3.15 9.5 114 5400 24 28 16515 -1)
      (103 volvo gas turbo four sedan rwd front 104.3 188.8 67.2 56.2 3045 ohc four 130 mpfi 3.62 3.15 7.5 162 5100 17 22 18420 -2)
      (74 volvo gas turbo four wagon rwd front 104.3 188.8 67.2 57.5 3157 ohc four 130 mpfi 3.62 3.15 7.5 162 5100 17 22 18950 -1)
      (95 volvo gas std four sedan rwd front 109.1 188.8 68.9 55.5 2952 ohc four 141 mpfi 3.78 3.15 9.5 114 5400 23 28 16845 -1)
      (95 volvo gas turbo four sedan rwd front 109.1 188.8 68.8 55.5 3049 ohc four 141 mpfi 3.78 3.15 8.7 160 5300 19 25 19045 -1)
      (95 volvo gas std four sedan rwd front 109.1 188.8 68.9 55.5 3012 ohcv six 173 mpfi 3.58 2.87 8.8 134 5500 18 23 21485 -1)
      (95 volvo diesel turbo four sedan rwd front 109.1 188.8 68.9 55.5 3217 ohc six 145 idi 3.01 3.4 23 106 4800 26 27 22470 -1)
      (95 volvo gas turbo four sedan rwd front 109.1 188.8 68.9 55.5 3062 ohc four 141 mpfi 3.78 3.15 9.5 114 5400 19 25 22625 -1)
      )))
