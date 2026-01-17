# Changelog

## dibble 0.3.2

CRAN release: 2026-01-16

- Support `.data` and `.env` pronouns in
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  ([\#30](https://github.com/UchidaMizuki/dibble/issues/30)).
- Replace [`unique()`](https://rdrr.io/r/base/unique.html) with
  [`vctrs::vec_unique()`](https://vctrs.r-lib.org/reference/vec_unique.html)
  for speed improvement.

## dibble 0.3.1

CRAN release: 2025-02-06

- Updated R version dependency to 4.3 to use matrixOps.
- Implemented
  [`pillar::tbl_sum()`](https://pillar.r-lib.org/reference/tbl_sum.html)
  for dibble ([\#20](https://github.com/UchidaMizuki/dibble/issues/20)).
- Fixed bugs in `all_equal_dim_names()`
  ([\#22](https://github.com/UchidaMizuki/dibble/issues/22)),
  [`rbind()`](https://rdrr.io/r/base/cbind.html)
  ([\#24](https://github.com/UchidaMizuki/dibble/issues/24)), and
  matrixOps ([\#27](https://github.com/UchidaMizuki/dibble/issues/27)).
- Fixed compatibility issue with purrr 1.0.4
  ([\#28](https://github.com/UchidaMizuki/dibble/issues/28)).

## dibble 0.3.0

CRAN release: 2024-06-23

- Change to preserve class in operations on dibbles
  ([\#13](https://github.com/UchidaMizuki/dibble/issues/13)).
- Implement a formatting system similar to pillar
  ([\#16](https://github.com/UchidaMizuki/dibble/issues/16)).
- Fix warning in
  [`broadcast()`](https://uchidamizuki.github.io/dibble/reference/broadcast.md)
  ([\#18](https://github.com/UchidaMizuki/dibble/issues/18)).
- Add tests and fix minor bugs.

## dibble 0.2.2

CRAN release: 2022-12-25

- Fix for dev purrr
  ([\#9](https://github.com/UchidaMizuki/dibble/issues/9)).

## dibble 0.2.1

CRAN release: 2022-08-07

- Broadcasts with transpositions are now warned.
- Resolve warning when checking equality of axis names.
- Fixed few bugs.

## dibble 0.2.0

CRAN release: 2022-05-29

- Override `base::%*%` and support matrix multiplications for dibbles.
- Override [`base::pmin()`](https://rdrr.io/r/base/Extremes.html) and
  [`base::pmax()`](https://rdrr.io/r/base/Extremes.html) functions.
- Add [`t()`](https://rdrr.io/r/base/t.html) methods for dibbles.
- Add
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  and `dplyr::rows_*()` methods for dibbles.
- Add .names_sep argument to dibble_by() and support dibble whose dim
  names are data frames.
- Add
  [`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html)
  methods for dibbles.

## dibble 0.1.1

CRAN release: 2022-03-16

- Support unary operation by `-`.
- Add [`solve()`](https://rdrr.io/r/base/solve.html) methods.
- Fix
  [`diag()`](https://uchidamizuki.github.io/dibble/reference/diag.md)
  actions.
- Add `list_sizes_unnamed()` helper to avoid retaining dim names
  ([\#4](https://github.com/UchidaMizuki/dibble/issues/4)).

## dibble 0.1.0

CRAN release: 2022-02-14

- This is a new release.
