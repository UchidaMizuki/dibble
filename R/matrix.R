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

#' @export
diag <- function(x, ...) {
  UseMethod("diag")
}

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

#' @export
diag.tbl_ddf <- function(x, axes, ...) {
  diag_dibble(x, axes, ...)
}

#' @export
diag.grouped_ddf <- function(x, axes, ...) {
  diag_dibble(x, axes, ...)
}

diag_dibble <- function(x, axes, ...) {
  diag(as_ddf_col(x), axes, ...)
}

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

#' @export
`diag<-` <- function(x, ..., value) {
  UseMethod("diag<-")
}

#' @export
`diag<-.default` <- function(x, ..., value) {
  base::`diag<-`(x, value)
}

#' @export
`diag<-.tbl_ddf` <- function(x, ..., value) {
  `diag<-_dibble`(x, ..., value)
}

#' @export
`diag<-.grouped_ddf` <- function(x, ..., value) {
  `diag<-_dibble`(x, ..., value)
}

`diag<-_dibble` <- function(x, ..., value) {
  `diag<-`(as_ddf_col(x), ..., value)
}

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
  if (is.null(dim_names_value)) {
    diag(x) <- vec_recycle(value, vec_size(dim_names[[1L]]))
  } else {
    diag(x) <- as.vector(value)[vec_match(dim_names[[1L]], dim_names_value[[1L]])]
  }
  new_ddf_col(x, dim_names)
}
