# Broadcast to a new dimension

Broadcasts the dimension of the object to a new dimension.

## Usage

``` r
broadcast(x, dim_names = NULL, ...)

# Default S3 method
broadcast(x, dim_names = NULL, ...)

# S3 method for class 'ddf_col'
broadcast(x, dim_names, ...)

# S3 method for class 'tbl_ddf'
broadcast(x, dim_names, ...)
```

## Arguments

- x:

  A dibble, vector, or array.

- dim_names:

  A character vector or list of dimension names.

- ...:

  Unused, for extensibility.

## Value

A dibble.

## Details

Operations between dibbles are automatically broadcasted, but for safety
reasons, warnings are issued. `broadcast()` can suppress the warnings if
`dim_names` matches the dimension of `x`.

## Examples

``` r
x <- broadcast(1:2,
               list(axis1 = letters[1:2]))
y <- broadcast(1:3,
               list(axis2 = letters[1:3]))
broadcast(x * y, c("axis1", "axis2"))
#> # A dibble:   6
#> # Dimensions: axis1 [2], axis2 [3]
#>   axis1 axis2     .
#>   <chr> <chr> <int>
#> 1 a     a         1
#> 2 a     b         2
#> 3 a     c         3
#> 4 b     a         2
#> 5 b     b         4
#> 6 b     c         6
```
