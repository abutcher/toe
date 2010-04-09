Beer_Combined(Gene+Clinical).csv
- This file contains all gene expression values (7096 genes, excluded few control genes)
  and patients coupled with the patients' clinical outcome (survival time and overall survival status)

Beer_3yr.csv
- This is file used for analysis.
- Using file above, assign class lable to each patient using following logic:
  - Survival time < 36 and overall-survival status = 0 ==> removed as they are censored cases
  - Survival time < 36 and overall-survival status = 1 ==> Class B
  - Survival time >= 36 and overall-survival status = 0 or 1 ==> Class A
