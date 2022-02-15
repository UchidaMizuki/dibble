#' @export
solve.ddf_col <- function(a, b, ...) {
  if (is_missing(b)) {
    dim_names <- dimnames(a)
    a <- undibble(a)
    new_ddf_col(unname(solve(a)), dim_names)
  } else {
    NextMethod()
  }
}

#' Matrix diagonals
#'
#' Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
#'
#' These functions override base functions to make them generic. The default
#' methods call the base versions.
#'
#' @param x A dibble, matrix, vector or 1D array, or missing.
#' @param ... Unused, for extensibility.
#'
#' @param nrow,ncol Optional dimensions for the result when x is not a matrix.
#' @param names (When x is a matrix) logical indicating if the resulting vector,
#' the diagonal of x, should inherit names from dimnames(x) if available.
#'
#' @param axes A character vector of axes.
#'
#' @param value Replacement values.
#'
#' @return A dibble if x is a dibble. See [base::diag()] for the return values
#' of the default methods.
#'
#' @name diag

#' @rdname diag
#' @export
diag <- function(x, ...) {
  UseMethod("diag")
}

#' @rdname diag
#' @export
diag.default <- function(x = 1, nrow, ncol,
                         names = TRUE, ...) {
  if (is.matrix(x)) {
    base::diag(x,
               names = names, ...)
  } else {
    base::diag(x, nrow, ncol, names)
  }
}

#' @rdname diag
#' @export
diag.tbl_ddf <- function(x, axes, ...) {
  wrap_dibble(diag)(x, axes, ...)
}

#' @rdname diag
#' @export
diag.grouped_ddf <- function(x, axes, ...) {
  wrap_dibble(diag)(x, axes, ...)
}

#' @rdname diag
#' @export
diag.ddf_col <- function(x, axes, ...) {
  old_dim_names <- dimnames(x)
  is_scalar_old_dim_names <- is_scalar_list(old_dim_names)
  stopifnot(
    is_scalar_old_dim_names || is_list(old_dim_names, 2L)
  )

  if (is_scalar_old_dim_names) {
    stopifnot(
      is_character(axes, 2L)
    )

    new_dim_names <- vec_c(old_dim_names, old_dim_names)
    names(new_dim_names) <- axes
  } else {
    stopifnot(
      is_scalar_character(axes),
      identical(old_dim_names[[1L]], old_dim_names[[2L]])
    )

    new_dim_names <- old_dim_names[1L]
    names(new_dim_names) <- axes
  }
  new_ddf_col(diag(as.array(x), ...),
              new_dim_names)
}

#' @rdname diag
#' @export
`diag<-` <- function(x, ..., value) {
  UseMethod("diag<-")
}

#' @rdname diag
#' @export
`diag<-.default` <- function(x, ..., value) {
  base::`diag<-`(x, value)
}

#' @rdname diag
#' @export
`diag<-.tbl_ddf` <- function(x, ..., value) {
  nm <- colnames(x)
  x <- wrap_dibble(`diag<-`)(x, ..., value)
  dibble(!!nm := x)
}

#' @rdname diag
#' @export
`diag<-.grouped_ddf` <- function(x, ..., value) {
  axes <- group_vars(x)
  nm <- colnames(x)
  x <- wrap_dibble(`diag<-`)(x, ..., value)
  x <- dibble(!!nm := x)
  group_by(x, dplyr::all_of(axes))
}

#' @rdname diag
#' @export
`diag<-.ddf_col` <- function(x, ..., value) {
  dim_names <- dimnames(x)
  dim_names_value <- dimnames(value)
  stopifnot(
    is_list(dim_names, 2L),
    identical(dim_names[[1L]], dim_names[[2L]]),
    is.null(dim_names_value) || is_scalar_list(dim_names_value)
  )

  x <- as.array(x)
  diag(x) <- as.vector(broadcast(value, dim_names[1L]))
  new_ddf_col(x, dim_names)
}
