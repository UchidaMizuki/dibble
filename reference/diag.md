# Matrix diagonals

Extract or replace the diagonal of a matrix, or construct a diagonal
matrix.

## Usage

``` r
diag(x, ...)

# Default S3 method
diag(x = 1, nrow, ncol, names, ...)

# S3 method for class 'tbl_ddf'
diag(x, axes, ...)

# S3 method for class 'ddf_col'
diag(x, axes, ...)

diag(x, ...) <- value

# Default S3 method
diag(x, ...) <- value

# S3 method for class 'tbl_ddf'
diag(x, ...) <- value

# S3 method for class 'ddf_col'
diag(x, ...) <- value
```

## Arguments

- x:

  A dibble, matrix, vector or 1D array, or missing.

- ...:

  Unused, for extensibility.

- nrow, ncol:

  Optional dimensions for the result when x is not a matrix.

- names:

  (When x is a matrix) logical indicating if the resulting vector, the
  diagonal of x, should inherit names from dimnames(x) if available.

- axes:

  A character vector of axes.

- value:

  Replacement values.

## Value

A dibble if x is a dibble. See
[`base::diag()`](https://rdrr.io/r/base/diag.html) for the return values
of the default methods.

## Details

These functions override base functions to make them generic. The
default methods call the base versions.
