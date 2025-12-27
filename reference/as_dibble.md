# Coerce an object to a dibble

`as_dibble()` turns an object into a dimensional data frame called a
dibble.

## Usage

``` r
as_dibble(x, ...)

# Default S3 method
as_dibble(x, ...)

# S3 method for class 'rowwise_df'
as_dibble(x, ...)

# S3 method for class 'grouped_df'
as_dibble(x, ...)

# S3 method for class 'ddf_col'
as_dibble(x, ...)

# S3 method for class 'tbl_ddf'
as_dibble(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Unused, for extensibility.

## Value

A dibble.
