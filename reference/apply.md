# Apply functions over array margins

Applying a function to margins of a dibble or array, including a matrix.

## Usage

``` r
apply(X, MARGIN, FUN, ...)

# Default S3 method
apply(X, MARGIN, FUN, ..., simplify = TRUE)

# S3 method for class 'tbl_ddf'
apply(X, MARGIN, FUN, ...)

# S3 method for class 'ddf_col'
apply(X, MARGIN, FUN, ...)
```

## Arguments

- X:

  A dibble or array, including a matrix.

- MARGIN:

  An integer or character vector giving the subscripts which the
  function will be applied over.

- FUN:

  A function to be applied.

- ...:

  Optional arguments to FUN.

- simplify:

  A logical indicating whether results should be simplified if possible.

## Value

A dibble if X is a dibble. See
[`base::apply()`](https://rdrr.io/r/base/apply.html) for the return
value of the default method.

## Details

`apply()` overrides [`base::apply()`](https://rdrr.io/r/base/apply.html)
to make it generic. The default method calls the base version.

## See also

[`base::apply()`](https://rdrr.io/r/base/apply.html).

## Examples

``` r
x <- array(1:24, 2:4,
           list(axis1 = letters[1:2],
                axis2 = letters[1:3],
                axis3 = letters[1:4]))

apply(x, 2:3, sum)
#>      axis3
#> axis2  a  b  c  d
#>     a  3 15 27 39
#>     b  7 19 31 43
#>     c 11 23 35 47
apply(as_dibble(x), 2:3, sum)
#> # A dibble:   12
#> # Dimensions: axis2 [3], axis3 [4]
#>    axis2 axis3     .
#>    <chr> <chr> <int>
#>  1 a     a         3
#>  2 a     b        15
#>  3 a     c        27
#>  4 a     d        39
#>  5 b     a         7
#>  6 b     b        19
#>  7 b     c        31
#>  8 b     d        43
#>  9 c     a        11
#> 10 c     b        23
#> 11 c     c        35
#> 12 c     d        47

apply(x, c("axis2", "axis3"), sum)
#>      axis3
#> axis2  a  b  c  d
#>     a  3 15 27 39
#>     b  7 19 31 43
#>     c 11 23 35 47
apply(as_dibble(x), c("axis2", "axis3"), sum)
#> # A dibble:   12
#> # Dimensions: axis2 [3], axis3 [4]
#>    axis2 axis3     .
#>    <chr> <chr> <int>
#>  1 a     a         3
#>  2 a     b        15
#>  3 a     c        27
#>  4 a     d        39
#>  5 b     a         7
#>  6 b     b        19
#>  7 b     c        31
#>  8 b     d        43
#>  9 c     a        11
#> 10 c     b        23
#> 11 c     c        35
#> 12 c     d        47
```
