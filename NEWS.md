# dibble 0.3.1

* Updated R version dependency to 4.3 to use matrixOps.
* Implemented `pillar::tbl_sum()` for dibble (#20).
* Fixed bugs in `all_equal_dim_names()` (#22), `rbind()` (#24), and matrixOps (#27).
* Fixed compatibility issue with purrr 1.0.4 (#28).

# dibble 0.3.0

* Change to preserve class in operations on dibbles (#13).
* Implement a formatting system similar to pillar (#16).
* Fix warning in `broadcast()` (#18).
* Add tests and fix minor bugs.

# dibble 0.2.2

* Fix for dev purrr (#9).

# dibble 0.2.1

* Broadcasts with transpositions are now warned.
* Resolve warning when checking equality of axis names.
* Fixed few bugs.

# dibble 0.2.0

* Override `base::%*%` and support matrix multiplications for dibbles.
* Override `base::pmin()` and `base::pmax()` functions.
* Add `t()` methods for dibbles.
* Add `dplyr::filter()` and `dplyr::rows_*()` methods for dibbles.
* Add .names_sep argument to dibble_by() and support dibble whose dim names are data frames.
* Add `tidyr::replace_na()` methods for dibbles.

# dibble 0.1.1

* Support unary operation by `-`.
* Add `solve()` methods.
* Fix `diag()` actions.
* Add `list_sizes_unnamed()` helper to avoid retaining dim names (#4).

# dibble 0.1.0

* This is a new release.
