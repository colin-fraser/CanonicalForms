
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CanonicalForms

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/colin-fraser/CanonicalForms/branch/master/graph/badge.svg)](https://app.codecov.io/gh/colin-fraser/CanonicalForms?branch=master)
[![R-CMD-check](https://github.com/colin-fraser/CanonicalForms/workflows/R-CMD-check/badge.svg)](https://github.com/colin-fraser/CanonicalForms/actions)
<!-- badges: end -->

`CanonicalForms` is an R package for ensuring that data sets conform to
an expected format.

## Installation

You can install the development version of CanonicalForms from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("colin-fraser/CanonicalForms")
```

## Basic Example

A `CanonicalForm` allows you to check whether a dataset conforms to an
expected format. The following code creates a simple CanonicalForm
object for the dataset `cars`, as well as a pair of copies of the
dataset, one that conforms to the expected format and one that does not.

``` r
library(CanonicalForms)

cf <- canonical_form(
  object_class = "data.frame",
  col_names = c("speed", "dist"),
  col_classes = c("numeric", "numeric")
)

passing <- cars
failing <- cars |> 
  setNames(c("speed", "distance"))

passing |> 
  is_canonical(cf)  # checking whether `passing` corresponds to the form specified in cf
#> CHECKS SUMMARY
#> check_class............................✔
#> check_col_names........................✔
#> check_col_classes......................✔
#> 
#> All checks passed 😎
#> 
#> [1] TRUE

failing |> 
  is_canonical(cf)
#> CHECKS SUMMARY
#> check_class............................✔
#> check_col_names........................x
#> check_col_classes......................✔
#> 
#> Additional information:
#> Failed check: check_col_names
#> `canonical`: "speed" "dist"    
#> `given`:     "speed" "distance"
#> 
#> [1] FALSE
```

## Extracting and storing a `CanonicalForm`

It can be a little bit tedious to type out the full canonical schema in
the way shown above, especially for datasets with a large number of
columns. For this reason, there is an `extract_canonical_form` function
which will use a dataset as a template to create a `CanonicalForm`
object, as well as a `to_r_code` method that writes the boilerplate code
for initializing a new CanonicalForm.

``` r
# the starwars dataset is a tibble with 14 columns
starwars <- dplyr::starwars
head(starwars)
#> # A tibble: 6 × 14
#>   name     height  mass hair_color  skin_color eye_color birth_year sex   gender
#>   <chr>     <int> <dbl> <chr>       <chr>      <chr>          <dbl> <chr> <chr> 
#> 1 Luke Sk…    172    77 blond       fair       blue            19   male  mascu…
#> 2 C-3PO       167    75 <NA>        gold       yellow         112   none  mascu…
#> 3 R2-D2        96    32 <NA>        white, bl… red             33   none  mascu…
#> 4 Darth V…    202   136 none        white      yellow          41.9 male  mascu…
#> 5 Leia Or…    150    49 brown       light      brown           19   fema… femin…
#> 6 Owen La…    178   120 brown, grey light      blue            52   male  mascu…
#> # … with 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>

# this uses the starwars dataset as a template to extract a CanonicalForm
cf <- extract_canonical_form(starwars)
to_r_code(cf) # and this writes the boilerplate R code to construct that form
#> canonical_form(
#>   object_class = c("tbl_df", "tbl", "data.frame"),
#>   col_names = c(
#>     "name", "height", "mass", "hair_color", "skin_color", "eye_color",
#>     "birth_year", "sex", "gender", "homeworld", "species", "films",
#>     "vehicles", "starships"
#>   ),
#>   col_classes = c(
#>     "character", "integer", "numeric", "character", "character",
#>     "character", "numeric", "character", "character", "character",
#>     "character", "list", "list", "list"
#>   ),
#>   transformers = list(),
#>   checks = list(check_class = function(x) {
#>     compare_vecs(canonical_object_class(), class(x))
#>   }, check_col_names = function(x) {
#>     compare_vecs(canonical_col_names(), colnames(x))
#>   }, check_col_classes = function(x) {
#>     compare_vecs(canonical_col_classes(), classes(x))
#>   }),
#>   add_default_checks = FALSE
#> )
```

## Checking if a dataset is Canonical during transformations

Suppose I have a pipeline that does the following transformations to the
`starwars` dataset.

Now I have another script where I’m trying performing the same
transformations. I can add a call to `check_canonical` at the end of the
transformations to make sure that the pipeline does what I expect.
`check_canonical` returns its input, but will raise a warning if the
checks fail.

``` r
starwars_small_2 <- starwars |> 
  select(name, height, mass) |> 
  check_canonical(swcf)
#> Warning: CHECKS SUMMARY
#> check_class............................✔
#> check_col_names........................x
#> check_col_classes......................x
#> 
#> Additional information:
#> Failed check: check_col_names
#> `canonical`: "NAME" "HEIGHT" "MASS"
#> `given`:     "name" "height" "mass"
#> 
#> Failed check: check_col_classes
#> `canonical`: "character" "integer" "integer"
#> `given`:     "character" "integer" "numeric"
#> 
```

You can also set it to raise an exception rather than a warning.

If the pipeline returns the expected format, nothing visible will
happen.

``` r
starwars_small_2 <- starwars |> 
  transmute(NAME = name, HEIGHT = height, MASS = as.integer(mass)) |> 
  check_canonical(swcf, behavior = 'stop')
head(starwars_small_2)
#> # A tibble: 6 × 3
#>   NAME           HEIGHT  MASS
#>   <chr>           <int> <int>
#> 1 Luke Skywalker    172    77
#> 2 C-3PO             167    75
#> 3 R2-D2              96    32
#> 4 Darth Vader       202   136
#> 5 Leia Organa       150    49
#> 6 Owen Lars         178   120
```

## Adding more checks

By default, a newly created `CanonicalForm` objects have three checks:
they’ll check that the type of dataset matches, the column names match,
and the column types match. The package also provides other checks that
can be run with `is_canonical` and `check_canonical`, and it’s easy to
write custom checks and add those as well.

``` r
# passing example
swcf2 <- swcf |> 
  add_checks(
    no_nas = check_no_nas(cols = c('NAME')),  # check that no NAME values are NA
    positive_values = check_greater_than(HEIGHT = 0, MASS = 0)  # check that HEIGHT and MASS are greater than 0
    )

starwars_small |> 
  check_canonical(swcf2)
#> # A tibble: 87 × 3
#>    NAME               HEIGHT  MASS
#>    <chr>               <int> <int>
#>  1 Luke Skywalker        172    77
#>  2 C-3PO                 167    75
#>  3 R2-D2                  96    32
#>  4 Darth Vader           202   136
#>  5 Leia Organa           150    49
#>  6 Owen Lars             178   120
#>  7 Beru Whitesun lars    165    75
#>  8 R5-D4                  97    32
#>  9 Biggs Darklighter     183    84
#> 10 Obi-Wan Kenobi        182    77
#> # … with 77 more rows

# failing example
swcf3 <- swcf |> 
  add_checks(no_nas = check_no_nas(c("NAME", "HEIGHT", "MASS")),
             min_values = check_greater_than(HEIGHT = 1000, MASS = 0))

starwars_small |> 
  check_canonical(swcf3)
#> Warning: CHECKS SUMMARY
#> check_class............................✔
#> check_col_names........................✔
#> check_col_classes......................✔
#> no_nas.................................x
#> min_values.............................x
#> 
#> Additional information:
#> Failed check: no_nas
#> Unexpected NAs in the following column(s):
#> x HEIGHT
#> x MASS
#> 
#> Failed check: min_values
#> Values found below minimum in the following column(s):
#> x HEIGHT
#> 
#> # A tibble: 87 × 3
#>    NAME               HEIGHT  MASS
#>    <chr>               <int> <int>
#>  1 Luke Skywalker        172    77
#>  2 C-3PO                 167    75
#>  3 R2-D2                  96    32
#>  4 Darth Vader           202   136
#>  5 Leia Organa           150    49
#>  6 Owen Lars             178   120
#>  7 Beru Whitesun lars    165    75
#>  8 R5-D4                  97    32
#>  9 Biggs Darklighter     183    84
#> 10 Obi-Wan Kenobi        182    77
#> # … with 77 more rows
```
