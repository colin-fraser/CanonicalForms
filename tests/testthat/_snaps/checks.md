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
      check_result
    Output
      failed check
      
      Additional info:
      x a
        Value(s) found below minimum of 0

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      x a
        Value(s) found below minimum of 0
      x b
        Value(s) found below minimum of 2

---

    Code
      check_canonical(df, cf)
    Warning <warning>
      CHECKS SUMMARY
      check_class............................✔
      check_col_names........................✔
      check_col_classes......................✔
      column_lower_bounds....................x
      
      Additional information:
      Failed check: column_lower_bounds
      x a
        Value(s) found below minimum of 0
      x b
        Value(s) found below minimum of 2
      
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
      x a
        Value(s) found above maximum of 0

---

    Code
      lt4(df)
    Output
      failed check
      
      Additional info:
      x a
        Value(s) found above maximum of -2
      x b
        Value(s) found above maximum of 0

# check between

    Code
      check_result
    Output
      failed check
      
      Additional info:
      x a
        Value found outside of (1, 4]

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      x b
        Value found outside of [3, 7)

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      x a
        Value found outside of (1, 4)
      x b
        Value found outside of (3, 7)

# test internal factor levels checker

    Code
      check_result
    Output
      failed check
      
      Additional info:
           canonical | given    
       [1] "Jan"     - "Feb" [2]
       [2] "Feb"     - "Jan" [3]
       [3] "Mar"     | "Mar" [4]
       [4] "Apr"     -          
       [5] "May"     -          
       [6] "Jun"     -          
       [7] "Jul"     -          
       [8] "Aug"     -          
       [9] "Sep"     -          
      [10] "Oct"     -          
      [11] "Nov"     -          
      [12] "Dec"     - "May" [5]

# check factor levels

    Code
      check_result
    Output
      failed check
      
      Additional info:
      x a
        Column is not a factor variable

---

    Code
      check_result
    Output
      failed check
      
      Additional info:
      x a
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
      x a
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
      x a
        `canonical[1:6]`: "Apr" "Aug" "Dec" "Feb" "Jan" "Jul"
        `given[1:5]`:     "Apr" "Aug"       "Feb" "Jan" "Jul"
      
    Output
      [1] FALSE

