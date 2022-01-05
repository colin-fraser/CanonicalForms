# no nas test

    Code
      no_nas(df)
    Output
      failed check
      
      Additional info:
      Unexpected NAs in the following column(s):
      x a
      x b

---

    Code
      no_nas2(df)
    Output
      failed check
      
      Additional info:
      Unexpected NAs in the following column(s):
      x a

# gt test

    Code
      check_canonical(df, cf)
    Warning <warning>
      CHECKS SUMMARY
      check_class............................✔
      check_col_names........................✔
      check_col_classes......................✔
      gt.....................................x
      
      Additional information:
      Failed check: gt
      Values found below minimum in the following column(s):
      x a
      
    Output
         a  b
      1 -1  1
      2  0  1
      3 NA NA

# less than

    Code
      lt1(df)
    Output
      passed check

---

    Code
      lt2(df)
    Output
      passed check

---

    Code
      lt3(df)
    Output
      failed check
      
      Additional info:
      Values found above maximum in the following column(s):
      x a

---

    Code
      lt4(df)
    Output
      failed check
      
      Additional info:
      Values found above maximum in the following column(s):
      x a

# check between

    Code
      check_result
    Output
      failed check
      
      Additional info:
      Values found below minimum in the following column(s):
      x a

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      
      Values found above maximum in the following column(s):
      x b

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      Values found below minimum in the following column(s):
      x a
      Values found above maximum in the following column(s):
      x b

# test internal factor levels checker

    Code
      check_result
    Output
      failed check
      
      Additional info:
           canonical | given    
       [1] "Apr"     | "Apr" [1]
       [2] "Aug"     - "Feb" [2]
       [3] "Dec"     - "Jan" [3]
       [4] "Feb"     - "Mar" [4]
       [5] "Jan"     - "May" [5]
       [6] "Jul"     -          
       [7] "Jun"     -          
       [8] "Mar"     -          
       [9] "May"     -          
      [10] "Nov"     -          
      [11] "Oct"     -          
      [12] "Sep"     -          

# check factor levels

    Code
      check_result
    Output
      failed check
      
      Additional info:
      Column is not a factor variable

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      `canonical[1:6]`: "Apr" "Aug" "Dec" "Feb" "Jan" "Jul"
      `given[1:5]`:     "Apr" "Aug"       "Feb" "Jan" "Jul"

---

    Code
      is_canonical(df, cf)
    Message <message>
      CHECKS SUMMARY
      check_class............................✔
      check_col_names........................✔
      check_col_classes......................✔
      check_factor_levels....................✔
      
      All checks passed 😎
      
    Output
      [1] TRUE

---

    Code
      is_canonical(df2, cf)
    Message <message>
      CHECKS SUMMARY
      check_class............................✔
      check_col_names........................✔
      check_col_classes......................x
      check_factor_levels....................x
      
      Additional information:
      Failed check: check_col_classes
      `canonical`: "factor"   
      `given`:     "character"
      
      Failed check: check_factor_levels
      Column is not a factor variable
      
    Output
      [1] FALSE

---

    Code
      is_canonical(df3, cf)
    Message <message>
      CHECKS SUMMARY
      check_class............................✔
      check_col_names........................✔
      check_col_classes......................✔
      check_factor_levels....................x
      
      Additional information:
      Failed check: check_factor_levels
      `canonical[1:6]`: "Apr" "Aug" "Dec" "Feb" "Jan" "Jul"
      `given[1:5]`:     "Apr" "Aug"       "Feb" "Jan" "Jul"
      
    Output
      [1] FALSE

