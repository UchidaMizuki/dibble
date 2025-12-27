# Row and column names

Retrieve or set the row or column names of a matrix-like object.

## Usage

``` r
rownames(x, ...)

# Default S3 method
rownames(x, ...)

# S3 method for class 'ddf_col'
rownames(x, ...)

# S3 method for class 'tbl_ddf'
rownames(x, ...)

colnames(x, ...)

# Default S3 method
colnames(x, ...)

# S3 method for class 'ddf_col'
colnames(x, ...)

# S3 method for class 'tbl_ddf'
colnames(x, ...)
```

## Arguments

- x:

  A matrix-like object.

- ...:

  Other arguments passed on to methods.

## Value

A list of row/column names.

## Details

These functions override base functions to make them generic. The
default methods call the base versions.
