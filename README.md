
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dibble

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dibble)](https://CRAN.R-project.org/package=dibble)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/UchidaMizuki/dibble/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/dibble/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A ‘dibble’ (derived from ‘dimensional tibble’) is a data frame
consisting of arrays with dimension names, known as data cubes. The
columns of the dibbles are classified into dimensions or measures, and
the operations on the measures are broadcasted by dimension names.

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
library(tidyr)
```

### Broadcasting

``` r
arr1 <- array(1:6, c(2, 3),
              list(axis1 = letters[1:2],
                   axis2 = letters[1:3]))
arr2 <- array(1:2, 2,
              list(axis2 = letters[1:2]))

try(arr1 * arr2)
#> Error in arr1 * arr2 : non-conformable arrays
```

``` r

ddf1 <- as_dibble(arr1)
ddf2 <- as_dibble(arr2)

ddf1 * ddf2
#> Warning: Broadcasting,
#> New axes, dim_names = c("axis1", "axis2")
#> New coordinates, 
#>  $ axis2: chr "c"
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         6
#> 3 a     c        NA
#> 4 b     a         2
#> 5 b     b         8
#> 6 b     c        NA
```

``` r

# You can use broadcast() to suppress the warnings.
broadcast(ddf1 * ddf2,
          dim_names = c("axis1", "axis2"))
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         6
#> 3 a     c        NA
#> 4 b     a         2
#> 5 b     b         8
#> 6 b     c        NA
```

### dplyr methods

dibble provides some dplyr methods as follows,

- `as_tibble()`: From tibble package
- `filter()`
- `mutate()`: Experimental
- `rename()`
- `select()` and `relocate()`
- `slice()`: Specify locations (a integer vector) for each dimension

### How to build a dibble

#### From a data.frame

``` r
df <- expand_grid(axis1 = letters[1:2],
                  axis2 = letters[1:2]) |> 
  mutate(value1 = row_number(),
         value2 = value1 * 2)

ddf <- df |> 
  dibble_by(axis1, axis2)

df
#> # A tibble: 4 × 4
#>   axis1 axis2 value1 value2
#>   <chr> <chr>  <int>  <dbl>
#> 1 a     a          1      2
#> 2 a     b          2      4
#> 3 b     a          3      6
#> 4 b     b          4      8
```

``` r
ddf
#> # A dibble:   4 x 2
#> # Dimensions: axis1 [2], axis2 [2]
#> # Measures:   value1, value2
#>   axis1 axis2 value1 value2
#>   <chr> <chr>  <int>  <dbl>
#> 1 a     a          1      2
#> 2 a     b          2      4
#> 3 b     a          3      6
#> 4 b     b          4      8
```

``` r

# You can access the measures from the dibble with `$`.
ddf$value1
#> # A dibble:   4
#> # Dimensions: axis1 [2], axis2 [2]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         2
#> 3 b     a         3
#> 4 b     b         4
```

#### From an array with dimension names or a vector

dibble provides some dplyr methods as follows,

``` r
# from an array with dimension names
arr <- array(1:4, c(2, 2),
             list(axis1 = letters[1:2],
                  axis2 = letters[1:2]))

ddf1 <- as_dibble(arr)

# from a vector
ddf2 <- broadcast(1:4,
                  list(axis1 = letters[1:2],
                       axis2 = letters[1:2]))

arr
#>      axis2
#> axis1 a b
#>     a 1 3
#>     b 2 4
```

``` r
ddf1
#> # A dibble:   4
#> # Dimensions: axis1 [2], axis2 [2]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 b     a         2
#> 4 b     b         4
```

``` r
ddf2
#> # A dibble:   4
#> # Dimensions: axis1 [2], axis2 [2]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         3
#> 3 b     a         2
#> 4 b     b         4
```
