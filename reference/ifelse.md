# Conditional element selection

Selects elements from either `yes` or `no` depending on whether `test`
is `TRUE` or `FALSE`.

## Usage

``` r
ifelse(test, yes, no, ...)

# Default S3 method
ifelse(test, yes, no, ...)

# S3 method for class 'tbl_ddf'
ifelse(test, yes, no, ...)

# S3 method for class 'ddf_col'
ifelse(test, yes, no, ...)
```

## Arguments

- test:

  An object which can be coerced to logical mode.

- yes:

  Return values for true elements of test.

- no:

  Return values for false elements of test.

- ...:

  Unused, for extensibility.

## Value

A dibble if test is a dibble. See
[`base::ifelse()`](https://rdrr.io/r/base/ifelse.html) for the return
value of the default method.

## Details

`ifelse()` overrides
[`base::ifelse()`](https://rdrr.io/r/base/ifelse.html) to make it
generic. The default method calls the base version.

## See also

[`base::ifelse()`](https://rdrr.io/r/base/ifelse.html).
