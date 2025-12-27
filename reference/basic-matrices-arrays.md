# Basic matrices and arrays

Create basic matrices and arrays.

## Usage

``` r
eye(x, ...)

# Default S3 method
eye(x, y = x, ...)

# S3 method for class 'matrix'
eye(x, ...)

# S3 method for class 'ddf_col'
eye(x, ...)

# S3 method for class 'tbl_ddf'
eye(x, ...)

ones(x, ...)

# Default S3 method
ones(x, y = x, ...)

# S3 method for class 'array'
ones(x, ...)

# S3 method for class 'ddf_col'
ones(x, ...)

# S3 method for class 'tbl_ddf'
ones(x, ...)

zeros(x, ...)

# Default S3 method
zeros(x, y = x, ...)

# S3 method for class 'array'
zeros(x, ...)

# S3 method for class 'ddf_col'
zeros(x, ...)

# S3 method for class 'tbl_ddf'
zeros(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Other arguments passed on to methods.

- y:

  A scalar integer.

## Value

A dibble if x is a dibble. Otherwise, returns a matrix or an array.

## Details

These functions override base functions to make them generic. The
default methods call the base versions.
