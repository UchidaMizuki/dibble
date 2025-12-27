# The number of rows/columns

`nrow()` and `ncol()` return the number of rows or columns present in x.

## Usage

``` r
nrow(x, ...)

# Default S3 method
nrow(x, ...)

# S3 method for class 'ddf_col'
nrow(x, ...)

# S3 method for class 'tbl_ddf'
nrow(x, ...)

ncol(x, ...)

# Default S3 method
ncol(x, ...)

# S3 method for class 'ddf_col'
ncol(x, ...)

# S3 method for class 'tbl_ddf'
ncol(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Other arguments passed on to methods.

## Value

An integer or `NULL`.

## Details

These functions override base functions to make them generic. The
default methods call the base versions.
