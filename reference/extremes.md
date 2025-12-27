# Maxima and Minima

Returns the parallel maxima and minima of the input values.

## Usage

``` r
pmax(..., na.rm = FALSE)

# Default S3 method
pmax(..., na.rm = FALSE)

# S3 method for class 'ddf_col'
pmax(..., na.rm = FALSE)

# S3 method for class 'tbl_ddf'
pmax(..., na.rm = FALSE)

pmin(..., na.rm = FALSE)

# Default S3 method
pmin(..., na.rm = FALSE)

# S3 method for class 'ddf_col'
pmin(..., na.rm = FALSE)

# S3 method for class 'tbl_ddf'
pmin(..., na.rm = FALSE)
```

## Arguments

- ...:

  Dibbles, numeric or character arguments.

- na.rm:

  a logical indicating whether missing values should be removed.

## Value

A dibble if `...` are dibbles. See
[`base::pmax()`](https://rdrr.io/r/base/Extremes.html) and
[`base::pmin()`](https://rdrr.io/r/base/Extremes.html) for the return
value of the default method.

## Details

These functions override base functions to make them generic. The
default methods call the base versions.

## See also

[`base::pmax()`](https://rdrr.io/r/base/Extremes.html),
[`base::pmin()`](https://rdrr.io/r/base/Extremes.html).
