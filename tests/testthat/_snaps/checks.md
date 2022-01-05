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

