
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dibble

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/dibble/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/dibble?branch=main)
[![R-CMD-check](https://github.com/UchidaMizuki/dibble/workflows/R-CMD-check/badge.svg)](https://github.com/UchidaMizuki/dibble/actions)
<!-- badges: end -->

A ‘dibble’ is a data frame consisting of arrays with named dimensions
(known as data cubes). The columns of the dibbles are classified into
dimensions or metrics, and the operations on the metrics are
broadcasted.

## Installation

``` r
# the released version from CRAN:
install.packages("dibble")

# the development version from GitHub:
# install.packages("devtools")
devtools::install_github("UchidaMizuki/dibble")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dibble)
library(dplyr)
```

A dibble can be created from a data frame using `dibble_by()` function.

``` r
ddf <- tidyr::expand_grid(axis1 = 1:2,
                          axis2 = 1:3) %>% 
  mutate(value = row_number()) %>% 
  dibble_by(axis1, axis2)

ddf
#> # A dibble:   6 x 1
#> # Dimensions: axis1 [2], axis2 [3]
#> # Metrics:    value
#>   axis1 axis2 value
#>   <int> <int> <int>
#> 1     1     1     1
#> 2     1     2     2
#> 3     1     3     3
#> 4     2     1     4
#> 5     2     2     5
#> 6     2     3     6
```

You can access the metrics from the dibble with `$`.

``` r
ddf$value
#> # A metric:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <int> <int> <int>
#> 1     1     1     1
#> 2     1     2     2
#> 3     1     3     3
#> 4     2     1     4
#> 5     2     2     5
#> 6     2     3     6
```

If the axes of one dibble encompasses the axes of another, `mutate()`
allows for dimension-preserving broadcasting.

``` r
ddf1 <- tidyr::expand_grid(axis1 = 1:2,
                           axis2 = 1:3) %>% 
  mutate(value = row_number()) %>% 
  dibble_by(axis1, axis2)

ddf2 <- tidyr::expand_grid(axis2 = 1:4) %>% 
  mutate(value = row_number()) %>% 
  dibble_by(axis2)

ddf1 %>% 
  mutate(value = value + ddf2$value)
#> # A dibble:   6 x 1
#> # Dimensions: axis1 [2], axis2 [3]
#> # Metrics:    value
#>   axis1 axis2 value
#>   <int> <int> <int>
#> 1     1     1     2
#> 2     1     2     4
#> 3     1     3     6
#> 4     2     1     5
#> 5     2     2     7
#> 6     2     3     9
```

`dibble()` allows broadcasting based on the union set of axes of
multiple dibbles (or metrics).

``` r
ddf1 <- tidyr::expand_grid(axis1 = 1:2,
                           axis2 = 1:3) %>% 
  mutate(value = row_number()) %>% 
  dibble_by(axis1, axis2)

ddf2 <- tidyr::expand_grid(axis2 = 1:4,
                           axis3 = 1:2) %>% 
  mutate(value = row_number()) %>% 
  dibble_by(axis2, axis3)

dibble(value = ddf1$value * ddf2$value)
#> # A dibble:   16 x 1
#> # Dimensions: axis1 [2], axis2 [4], axis3 [2]
#> # Metrics:    value
#>    axis1 axis2 axis3 value
#>    <int> <int> <int> <int>
#>  1     1     1     1     1
#>  2     1     1     2     2
#>  3     1     2     1     6
#>  4     1     2     2     8
#>  5     1     3     1    15
#>  6     1     3     2    18
#>  7     1     4     1    NA
#>  8     1     4     2    NA
#>  9     2     1     1     4
#> 10     2     1     2     8
#> 11     2     2     1    15
#> 12     2     2     2    20
#> 13     2     3     1    30
#> 14     2     3     2    36
#> 15     2     4     1    NA
#> 16     2     4     2    NA
```
