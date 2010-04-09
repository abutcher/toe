;%
;% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;%
;% Identification code deleted.
;%
;% As used by Kilpatrick, D. & Cameron-Jones, M. (1998). Numeric prediction
;% using instance-based learning with encoding length selection. In Progress
;% in Connectionist-Based Information Systems. Singapore: Springer-Verlag.
;%
;% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;%
;% NAME:  LOW BIRTH WEIGHT DATA
;% KEYWORDS:  Logistic Regression
;% SIZE:  189 observations, 11 variables
;%
;% NOTE:
;%         These data come from Appendix 1 of Hosmer and Lemeshow (1989).
;% These data are copyrighted and must be acknowledged and used accordingly.
;%
;% DESCRIPTIVE ABSTRACT:
;%         The goal of this study was to identify risk factors associated with
;% giving birth to a low birth weight baby (weighing less than 2500 grams).
;% Data were collected on 189 women, 59 of which had low birth weight babies
;% and 130 of which had normal birth weight babies.  Four variables which were
;% thought to be of importance were age, weight of the subject at her last
;% menstrual period, race, and the number of physician visits during the first
;% trimester of pregnancy.
;%
;%
;% SOURCE:
;%          Data were collected at Baystate Medical Center, Springfield,
;% Massachusetts, during 1986.
;%
;%
;% NOTE:
;%           This data set consists of the complete data.  A paired data set
;% created from this low birth weight data may be found in plowbwt.dat and
;% a 3 to 1 matched data set created from the low birth weight data may be
;% found in mlowbwt.dat.
;%
;%
;%
;% Table:  Code Sheet for the Variables in the Low Birth Weight Data Set.
;%
;% Columns   Variable                                              Abbreviation
;% -----------------------------------------------------------------------------
;% 2-4     Identification Code                                     ID
;%
;% 10      Low Birth Weight (0 = Birth Weight ge 2500g,            LOW
;%                           l = Birth Weight < 2500g)
;%
;% 17-18   Age of the Mother in Years                              AGE
;%
;% 23-25   Weight in Pounds at the Last Menstrual Period           LWT
;%
;% 32      Race (1 = White, 2 = Black, 3 = Other)                  RACE
;%
;% 40      Smoking Status During Pregnancy (1 = Yes, 0 = No)       SMOKE
;%
;% 48      History of Premature Labor (0 = None, 1 = One, etc.)    PTL
;%
;% 55      History of Hypertension (1 = Yes, 0 = No)               HT
;%
;% 61      Presence of Uterine Irritability (1 = Yes, 0 = No)      UI
;%
;% 67      Number of Physician Visits During the First Trimester   FTV
;%                 (0 = None, 1 = One, 2 = Two, etc.)
;%
;% 73-76   Birth Weight in Grams                                   BWT
;% -----------------------------------------------------------------------------
;%
;% PEDAGOGICAL NOTES:
;%         These data have been used as an example of fitting a multiple
;% logistic regression model.
;%
;% STORY BEHIND THE DATA:
;%         Low birth weight is an outcome that has been of concern to physicians
;% for years. This is due to the fact that infant mortality rates and birth
;% defect rates are very high for low birth weight babies. A woman's behavior
;% during pregnancy (including diet, smoking habits, and receiving prenatal care)
;% can greatly alter the chances of carrying the baby to term and, consequently,
;% of delivering a baby of normal birth weight.
;%         The variables identified in the code sheet given in the table have been
;% shown to be associated with low birth weight in the obstetrical literature. The
;% goal of the current study was to ascertain if these variables were important
;% in the population being served by the medical center where the data were
;% collected.
;%
;%
;% References:
;%
;% 1. Hosmer and Lemeshow, Applied Logistic Regression, Wiley, (1989).
;%
(defun g3lowbwt ()
  (data
    :name 'g3lowbwt
    :columns '($LOW $AGE $LWT $RACE $SMOKE $PTL $HT $UI $FTV $class)
    :egs
    '(
      (0 19 182 2 0 0 0 1 0 2523)
      (0 33 155 3 0 0 0 0 3 2551)
      (0 20 105 1 1 0 0 0 1 2557)
      (0 21 108 1 1 0 0 1 2 2594)
      (0 18 107 1 1 0 0 1 0 2600)
      (0 21 124 3 0 0 0 0 0 2622)
      (0 22 118 1 0 0 0 0 1 2637)
      (0 17 103 3 0 0 0 0 1 2637)
      (0 29 123 1 1 0 0 0 1 2663)
      (0 26 113 1 1 0 0 0 0 2665)
      (0 19 95 3 0 0 0 0 0 2722)
      (0 19 150 3 0 0 0 0 1 2733)
      (0 22 95 3 0 0 1 0 0 2750)
      (0 30 107 3 0 1 0 1 2 2750)
      (0 18 100 1 1 0 0 0 0 2769)
      (0 18 100 1 1 0 0 0 0 2769)
      (0 15 98 2 0 0 0 0 0 2778)
      (0 25 118 1 1 0 0 0 3 2782)
      (0 20 120 3 0 0 0 1 0 2807)
      (0 28 120 1 1 0 0 0 1 2821)
      (0 32 121 3 0 0 0 0 2 2835)
      (0 31 100 1 0 0 0 1 3 2835)
      (0 36 202 1 0 0 0 0 1 2836)
      (0 28 120 3 0 0 0 0 0 2863)
      (0 25 120 3 0 0 0 1 2 2877)
      (0 28 167 1 0 0 0 0 0 2877)
      (0 17 122 1 1 0 0 0 0 2906)
      (0 29 150 1 0 0 0 0 2 2920)
      (0 26 168 2 1 0 0 0 0 2920)
      (0 17 113 2 0 0 0 0 1 2920)
      (0 17 113 2 0 0 0 0 1 2920)
      (0 24 90 1 1 1 0 0 1 2948)
      (0 35 121 2 1 1 0 0 1 2948)
      (0 25 155 1 0 0 0 0 1 2977)
      (0 25 125 2 0 0 0 0 0 2977)
      (0 29 140 1 1 0 0 0 2 2977)
      (0 19 138 1 1 0 0 0 2 2977)
      (0 27 124 1 1 0 0 0 0 2992)
      (0 31 215 1 1 0 0 0 2 3005)
      (0 33 109 1 1 0 0 0 1 3033)
      (0 21 185 2 1 0 0 0 2 3042)
      (0 19 189 1 0 0 0 0 2 3062)
      (0 23 130 2 0 0 0 0 1 3062)
      (0 21 160 1 0 0 0 0 0 3062)
      (0 18 90 1 1 0 0 1 0 3076)
      (0 18 90 1 1 0 0 1 0 3076)
      (0 32 132 1 0 0 0 0 4 3080)
      (0 19 132 3 0 0 0 0 0 3090)
      (0 24 115 1 0 0 0 0 2 3090)
      (0 22 85 3 1 0 0 0 0 3090)
      (0 22 120 1 0 0 1 0 1 3100)
      (0 23 128 3 0 0 0 0 0 3104)
      (0 22 130 1 1 0 0 0 0 3132)
      (0 30 95 1 1 0 0 0 2 3147)
      (0 19 115 3 0 0 0 0 0 3175)
      (0 16 110 3 0 0 0 0 0 3175)
      (0 21 110 3 1 0 0 1 0 3203)
      (0 30 153 3 0 0 0 0 0 3203)
      (0 20 103 3 0 0 0 0 0 3203)
      (0 17 119 3 0 0 0 0 0 3225)
      (0 17 119 3 0 0 0 0 0 3225)
      (0 23 119 3 0 0 0 0 2 3232)
      (0 24 110 3 0 0 0 0 0 3232)
      (0 28 140 1 0 0 0 0 0 3234)
      (0 26 133 3 1 2 0 0 0 3260)
      (0 20 169 3 0 1 0 1 1 3274)
      (0 24 115 3 0 0 0 0 2 3274)
      (0 28 250 3 1 0 0 0 6 3303)
      (0 20 141 1 0 2 0 1 1 3317)
      (0 22 158 2 0 1 0 0 2 3317)
      (0 22 112 1 1 2 0 0 0 3317)
      (0 31 150 3 1 0 0 0 2 3321)
      (0 23 115 3 1 0 0 0 1 3331)
      (0 16 112 2 0 0 0 0 0 3374)
      (0 16 135 1 1 0 0 0 0 3374)
      (0 18 229 2 0 0 0 0 0 3402)
      (0 25 140 1 0 0 0 0 1 3416)
      (0 32 134 1 1 1 0 0 4 3430)
      (0 20 121 2 1 0 0 0 0 3444)
      (0 23 190 1 0 0 0 0 0 3459)
      (0 22 131 1 0 0 0 0 1 3460)
      (0 32 170 1 0 0 0 0 0 3473)
      (0 30 110 3 0 0 0 0 0 3475)
      (0 20 127 3 0 0 0 0 0 3487)
      (0 23 123 3 0 0 0 0 0 3544)
      (0 17 120 3 1 0 0 0 0 3572)
      (0 19 105 3 0 0 0 0 0 3572)
      (0 23 130 1 0 0 0 0 0 3586)
      (0 36 175 1 0 0 0 0 0 3600)
      (0 22 125 1 0 0 0 0 1 3614)
      (0 24 133 1 0 0 0 0 0 3614)
      (0 21 134 3 0 0 0 0 2 3629)
      (0 19 235 1 1 0 1 0 0 3629)
      (0 25 95 1 1 3 0 1 0 3637)
      (0 16 135 1 1 0 0 0 0 3643)
      (0 29 135 1 0 0 0 0 1 3651)
      (0 29 154 1 0 0 0 0 1 3651)
      (0 19 147 1 1 0 0 0 0 3651)
      (0 19 147 1 1 0 0 0 0 3651)
      (0 30 137 1 0 0 0 0 1 3699)
      (0 24 110 1 0 0 0 0 1 3728)
      (0 19 184 1 1 0 1 0 0 3756)
      (0 24 110 3 0 1 0 0 0 3770)
      (0 23 110 1 0 0 0 0 1 3770)
      (0 20 120 3 0 0 0 0 0 3770)
      (0 25 241 2 0 0 1 0 0 3790)
      (0 30 112 1 0 0 0 0 1 3799)
      (0 22 169 1 0 0 0 0 0 3827)
      (0 18 120 1 1 0 0 0 2 3856)
      (0 16 170 2 0 0 0 0 4 3860)
      (0 32 186 1 0 0 0 0 2 3860)
      (0 18 120 3 0 0 0 0 1 3884)
      (0 29 130 1 1 0 0 0 2 3884)
      (0 33 117 1 0 0 0 1 1 3912)
      (0 20 170 1 1 0 0 0 0 3940)
      (0 28 134 3 0 0 0 0 1 3941)
      (0 14 135 1 0 0 0 0 0 3941)
      (0 28 130 3 0 0 0 0 0 3969)
      (0 25 120 1 0 0 0 0 2 3983)
      (0 16 95 3 0 0 0 0 1 3997)
      (0 20 158 1 0 0 0 0 1 3997)
      (0 26 160 3 0 0 0 0 0 4054)
      (0 21 115 1 0 0 0 0 1 4054)
      (0 22 129 1 0 0 0 0 0 4111)
      (0 25 130 1 0 0 0 0 2 4153)
      (0 31 120 1 0 0 0 0 2 4167)
      (0 35 170 1 0 1 0 0 1 4174)
      (0 19 120 1 1 0 0 0 0 4238)
      (0 24 116 1 0 0 0 0 1 4593)
      (0 45 123 1 0 0 0 0 1 4990)
      (1 28 120 3 1 1 0 1 0 709)
      (1 29 130 1 0 0 0 1 2 1021)
      (1 34 187 2 1 0 1 0 0 1135)
      (1 25 105 3 0 1 1 0 0 1330)
      (1 25 85 3 0 0 0 1 0 1474)
      (1 27 150 3 0 0 0 0 0 1588)
      (1 23 97 3 0 0 0 1 1 1588)
      (1 24 128 2 0 1 0 0 1 1701)
      (1 24 132 3 0 0 1 0 0 1729)
      (1 21 165 1 1 0 1 0 1 1790)
      (1 32 105 1 1 0 0 0 0 1818)
      (1 19 91 1 1 2 0 1 0 1885)
      (1 25 115 3 0 0 0 0 0 1893)
      (1 16 130 3 0 0 0 0 1 1899)
      (1 25 92 1 1 0 0 0 0 1928)
      (1 20 150 1 1 0 0 0 2 1928)
      (1 21 200 2 0 0 0 1 2 1928)
      (1 24 155 1 1 1 0 0 0 1936)
      (1 21 103 3 0 0 0 0 0 1970)
      (1 20 125 3 0 0 0 1 0 2055)
      (1 25 89 3 0 2 0 0 1 2055)
      (1 19 102 1 0 0 0 0 2 2082)
      (1 19 112 1 1 0 0 1 0 2084)
      (1 26 117 1 1 1 0 0 0 2084)
      (1 24 138 1 0 0 0 0 0 2100)
      (1 17 130 3 1 1 0 1 0 2125)
      (1 20 120 2 1 0 0 0 3 2126)
      (1 22 130 1 1 1 0 1 1 2187)
      (1 27 130 2 0 0 0 1 0 2187)
      (1 20 80 3 1 0 0 1 0 2211)
      (1 17 110 1 1 0 0 0 0 2225)
      (1 25 105 3 0 1 0 0 1 2240)
      (1 20 109 3 0 0 0 0 0 2240)
      (1 18 148 3 0 0 0 0 0 2282)
      (1 18 110 2 1 1 0 0 0 2296)
      (1 20 121 1 1 1 0 1 0 2296)
      (1 21 100 3 0 1 0 0 4 2301)
      (1 26 96 3 0 0 0 0 0 2325)
      (1 31 102 1 1 1 0 0 1 2353)
      (1 15 110 1 0 0 0 0 0 2353)
      (1 23 187 2 1 0 0 0 1 2367)
      (1 20 122 2 1 0 0 0 0 2381)
      (1 24 105 2 1 0 0 0 0 2381)
      (1 15 115 3 0 0 0 1 0 2381)
      (1 23 120 3 0 0 0 0 0 2395)
      (1 30 142 1 1 1 0 0 0 2410)
      (1 22 130 1 1 0 0 0 1 2410)
      (1 17 120 1 1 0 0 0 3 2414)
      (1 23 110 1 1 1 0 0 0 2424)
      (1 17 120 2 0 0 0 0 2 2438)
      (1 26 154 3 0 1 1 0 1 2442)
      (1 20 105 3 0 0 0 0 3 2450)
      (1 26 190 1 1 0 0 0 0 2466)
      (1 14 101 3 1 1 0 0 0 2466)
      (1 28 95 1 1 0 0 0 2 2466)
      (1 14 100 3 0 0 0 0 2 2495)
      (1 23 94 3 1 0 0 0 0 2495)
      (1 17 142 2 0 0 1 0 0 2495)
      (1 21 130 1 1 0 1 0 3 2495)
      )))
