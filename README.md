
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dibble

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/dibble/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/dibble?branch=main)
[![R-CMD-check](https://github.com/UchidaMizuki/dibble/workflows/R-CMD-check/badge.svg)](https://github.com/UchidaMizuki/dibble/actions)
<!-- badges: end -->

A ‘dibble’ (derived from ‘dimensional tibble’) is a data frame
consisting of arrays with named dimensions (known as data cubes). The
columns of the dibbles are classified into dimensions or measures, and
the operations on the measures are broadcasted.

## Installation

``` r
# the released version from CRAN:
install.packages("dibble")

# the development version from GitHub:
# install.packages("devtools")
devtools::install_github("UchidaMizuki/dibble")
```

## Examples

``` r
library(dibble)
library(dplyr)
```

### Broadcasting

``` r
arr1 <- array(1:6, c(2, 3),
              list(axis1 = letters[1:2],
                   axis2 = letters[1:3]))
arr2 <- array(1:20, c(4, 5),
              list(axis2 = letters[1:4],
                   axis3 = letters[1:5]))

try(arr1 * arr2)
#> Error in arr1 * arr2 : non-conformable arrays

ddf1 <- as_dibble(arr1)
ddf2 <- as_dibble(arr2)

ddf1 * ddf2
#> Warning: Broadcasting,
#> New axes, dim_names = c("axis1", "axis2", "axis3")
#> New coordinates, 
#>  $ axis2: chr "d"
#> Warning: Broadcasting,
#> New axes, dim_names = c("axis1", "axis2", "axis3")
#> # A dibble:   40
#> # Dimensions: axis1 [2], axis2 [4], axis3 [5]
#>    axis1 axis2 axis3     .
#>    <chr> <chr> <chr> <int>
#>  1 a     a     a         1
#>  2 a     a     b         5
#>  3 a     a     c         9
#>  4 a     a     d        13
#>  5 a     a     e        17
#>  6 a     b     a         6
#>  7 a     b     b        18
#>  8 a     b     c        30
#>  9 a     b     d        42
#> 10 a     b     e        54
#> # ... with 30 more rows

# You can use broadcast() to suppress the warnings.
broadcast(ddf1 * ddf2,
          dim_names = c("axis1", "axis2", "axis3"))
#> # A dibble:   40
#> # Dimensions: axis1 [2], axis2 [4], axis3 [5]
#>    axis1 axis2 axis3     .
#>    <chr> <chr> <chr> <int>
#>  1 a     a     a         1
#>  2 a     a     b         5
#>  3 a     a     c         9
#>  4 a     a     d        13
#>  5 a     a     e        17
#>  6 a     b     a         6
#>  7 a     b     b        18
#>  8 a     b     c        30
#>  9 a     b     d        42
#> 10 a     b     e        54
#> # ... with 30 more rows
```

### How to build a dibble

#### From a data.frame

``` r
df <- expand.grid(axis1 = letters[1:2],
                  axis2 = letters[1:3]) %>% 
  as_tibble() %>% 
  mutate(value1 = row_number(),
         value2 = value1 * 2)

ddf <- df %>% 
  dibble_by(axis1, axis2)

df
#> # A tibble: 6 x 4
#>   axis1 axis2 value1 value2
#>   <fct> <fct>  <int>  <dbl>
#> 1 a     a          1      2
#> 2 b     a          2      4
#> 3 a     b          3      6
#> 4 b     b          4      8
#> 5 a     c          5     10
#> 6 b     c          6     12
ddf
#> # A dibble:   6 x 2
#> # Dimensions: axis1 [2], axis2 [3]
#> # Measures:   value1, value2
#>   axis1 axis2 value1 value2
#>   <fct> <fct>  <int>  <dbl>
#> 1 a     a          1      2
#> 2 a     b          3      6
#> 3 a     c          5     10
#> 4 b     a          2      4
#> 5 b     b          4      8
#> 6 b     c          6     12

# You can access the measures from the dibble with `$`.
ddf$value1
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <fct> <fct> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 a     c         5
#> 4 b     a         2
#> 5 b     b         4
#> 6 b     c         6
```

#### From an array with named dimensions

``` r
arr <- array(1:6, 2:3,
             list(axis1 = letters[1:2],
                  axis2 = letters[1:3]))

ddf <- as_dibble(arr)

arr
#>      axis2
#> axis1 a b c
#>     a 1 3 5
#>     b 2 4 6
ddf
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 a     c         5
#> 4 b     a         2
#> 5 b     b         4
#> 6 b     c         6
```

#### From a vector

``` r
ddf <- dibble(value = 1:6,
              .dim_names = list(axis1 = letters[1:2],
                                axis2 = letters[1:3]))
ddf
#> # A dibble:   6 x 1
#> # Dimensions: axis1 [2], axis2 [3]
#> # Measures:   value
#>   axis1 axis2 value
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 a     c         5
#> 4 b     a         2
#> 5 b     b         4
#> 6 b     c         6
ddf$value
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 a     c         5
#> 4 b     a         2
#> 5 b     b         4
#> 6 b     c         6
```

``` r
ddf <- broadcast(1:6,
                 list(axis1 = letters[1:2],
                      axis2 = letters[1:3]))

ddf
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 a     c         5
#> 4 b     a         2
#> 5 b     b         4
#> 6 b     c         6
```
