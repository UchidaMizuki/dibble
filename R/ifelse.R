#' Conditional element selection
#'
#' Selects elements from either `yes` or `no` depending on whether `test` is
#' `TRUE` or `FALSE`.
#'
#' `ifelse()` overrides [base::ifelse()] to make it generic. The default method
#' calls the base version.
#'
#' @param test An object which can be coerced to logical mode.
#' @param yes Return values for true elements of test.
#' @param no Return values for false elements of test.
#' @param ... Unused, for extensibility.
#'
#' @return A dibble if test is a dibble. See [base::ifelse()] for the return
#' value of the default method.
#'
#' @seealso [base::ifelse()].
#'
#' @export
ifelse <- function(test, yes, no, ...) {
  UseMethod("ifelse")
}

#' @rdname ifelse
#' @export
ifelse.default <- function(test, yes, no, ...) {
  base::ifelse(test, yes, no)
}

#' @rdname ifelse
#' @export
ifelse.tbl_ddf <- function(test, yes, no, ...) {
  ifelse_dibble(test, yes, no)
}

#' @rdname ifelse
#' @export
ifelse.grouped_ddf <- function(test, yes, no, ...) {
  ifelse_dibble(test, yes, no)
}

ifelse_dibble <- function(test, yes, no) {
  ifelse(as_ddf_col(test), yes, no)
}

#' @rdname ifelse
#' @export
ifelse.ddf_col <- function(test, yes, no, ...) {
  dim_names <- union_dim_names(dimnames(test), dimnames(yes), dimnames(no))
  test <- as.array(broadcast(test, dim_names))
  yes <- as.array(broadcast(yes, dim_names))
  no <- as.array(broadcast(no, dim_names))

  new_ddf_col(ifelse(test, yes, no),
              dim_names)
}
