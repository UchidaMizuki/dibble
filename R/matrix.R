#' Matrix Multiplication
#'
#' Multiplies two matrices, if they are conformable.
#'
#' `%*%` overrides [`base::%*%`] to make it generic. The default method
#' calls the base version.
#'
#' @param x Numeric or complex dibble, matrices or vectors.
#' @param y Numeric or complex dibble, matrices or vectors.
#'
#' @return A dibble if x or y is a dibble of a matrix. A scalar numeric if both
#' x and y are dibbles of vectors. See [`base::%*%`] for the return value of the
#' default method.
#'
#' @seealso [`base::%*%`]
#'
#' @export
`%*%` <- function(x, y) {
  UseMethod("%*%")
}

#' @export
`%*%.default` <- function(x, y) {
  base::`%*%`(x, y)
}

#' @export
`%*%.tbl_ddf` <- function(x, y) {
  matmult_dibble(x, y)
}

#' @export
`%*%.ddf_col` <- function(x, y) {
  matmult_dibble(x, y)
}

matmult_dibble <- function(x, y) {
  x <- as_ddf_col(x)
  y <- as_ddf_col(y)

  class <- class(x)
  dim_names_x <- dimnames(x)
  dim_names_y <- dimnames(y)

  if (vec_size(dim_names_x) == 1L) {
    x <- as.vector(x)
    dim_names_x <- NULL
  } else {
    x <- as.matrix(x)
    dim_names_x <- dim_names_x[1L]
  }

  if (vec_size(dim_names_y) == 1L) {
    y <- as.vector(y)
    dim_names_y <- NULL
  } else {
    y <- as.matrix(y)
    dim_names_y <- dim_names_y[2L]
  }

  new_dim_names <- purrr::compact(c(dim_names_x, dim_names_y))

  out <- x %*% y

  if (vec_is_empty(new_dim_names)) {
    as.vector(out)
  } else {
    dim(out) <- list_sizes_unnamed(new_dim_names)
    new_ddf_col(out, new_dim_names,
                class = class)
  }
}

#' @export
t.tbl_ddf <- function(x) {
  new_tbl_ddf(purrr::modify(undibble(x), t),
              rev(dimnames(x)),
              class = class(x))
}

#' @export
t.ddf_col <- function(x) {
  new_ddf_col(t(undibble(x)),
              rev(dimnames(x)),
              class = class(x))
}

#' @export
solve.tbl_ddf <- function(a, b, ...) {
  if (is_missing(b)) {
    wrap_dibble(solve)(a, ...)
  } else {
    wrap_dibble(solve)(a, b, ...)
  }
}

#' @export
solve.ddf_col <- function(a, b, ...) {
  if (is_missing(b)) {
    dim_names <- dimnames(a)
    class <- class(a)
    a <- undibble(a)
    new_ddf_col(unname(solve(a)), rev(dim_names),
                class = class)
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
diag.default <- function(x = 1, nrow, ncol, names, ...) {
  args <- list(x = x)

  if (!is_missing(nrow)) {
    args <- c(args,
              list(nrow = nrow))
  }

  if (!is_missing(ncol)) {
    args <- c(args,
              list(ncol = ncol))
  }

  if (!is_missing(names)) {
    args <- c(args,
              list(names = names))
  }

  exec(base::diag, !!!args)
}

#' @rdname diag
#' @export
diag.tbl_ddf <- function(x, axes, ...) {
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
      all_equal_dim_names(old_dim_names[[1L]], old_dim_names[[2L]])
    )

    new_dim_names <- old_dim_names[1L]
    names(new_dim_names) <- axes
  }
  new_ddf_col(diag(as.array(x), ...),
              new_dim_names,
              class = class(x))
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
  x <- wrap_dibble(`diag<-`)(x, ...,
                             value = value)
  dibble(!!nm := x)
}

#' @rdname diag
#' @export
`diag<-.ddf_col` <- function(x, ..., value) {
  dim_names <- dimnames(x)
  dim_names_value <- dimnames(value)
  stopifnot(
    is_list(dim_names, 2L),
    all_equal_dim_names(dim_names[[1L]], dim_names[[2L]]),
    is.null(dim_names_value) || is_scalar_list(dim_names_value)
  )

  class <- class(x)
  x <- as.array(x)
  diag(x) <- as.vector(broadcast(value, dim_names[1L]))
  new_ddf_col(x, dim_names,
              class = class)
}

#' Basic matrices and arrays
#'
#' Create basic matrices and arrays.
#'
#' These functions override base functions to make them generic. The default
#' methods call the base versions.
#'
#' @param x An object.
#' @param y A scalar integer.
#' @param ... Other arguments passed on to methods.
#'
#' @return A dibble if x is a dibble. Otherwise, returns a matrix or an array.
#'
#' @export
#' @name basic-matrices-arrays

#' @rdname basic-matrices-arrays
#' @export
eye <- function(x, ...) {
  UseMethod("eye")
}

#' @rdname basic-matrices-arrays
#' @export
eye.default <- function(x,
                        y = x, ...) {
  base::diag(nrow = x,
             ncol = y)
}

#' @rdname basic-matrices-arrays
#' @export
eye.matrix <- function(x, ...) {
  dim <- dim(x)
  out <- base::diag(nrow = dim[[1L]],
                    ncol = dim[[2L]])
  dimnames(out) <- dimnames(x)
  out
}

#' @rdname basic-matrices-arrays
#' @export
eye.ddf_col <- function(x, ...) {
  wrap_ddf_col(eye, matrix = TRUE)(x, ...)
}

#' @rdname basic-matrices-arrays
#' @export
eye.tbl_ddf <- function(x, ...) {
  wrap_dibble(eye)(x, ...)
}

#' @rdname basic-matrices-arrays
#' @export
ones <- function(x, ...) {
  UseMethod("ones")
}

#' @rdname basic-matrices-arrays
#' @export
ones.default <- function(x,
                         y = x, ...) {
  matrix(1, x, y)
}

#' @rdname basic-matrices-arrays
#' @export
ones.array <- function(x, ...) {
  array(1, dim(x), dimnames(x))
}

#' @rdname basic-matrices-arrays
#' @export
ones.ddf_col <- function(x, ...) {
  wrap_ddf_col(ones)(x, ...)
}

#' @rdname basic-matrices-arrays
#' @export
ones.tbl_ddf <- function(x, ...) {
  wrap_dibble(ones)(x, ...)
}

#' @rdname basic-matrices-arrays
#' @export
zeros <- function(x, ...) {
  UseMethod("zeros")
}

#' @rdname basic-matrices-arrays
#' @export
zeros.default <- function(x,
                          y = x, ...) {
  matrix(0, x, y)
}

#' @rdname basic-matrices-arrays
#' @export
zeros.array <- function(x, ...) {
  array(0, dim(x), dimnames(x))
}

#' @rdname basic-matrices-arrays
#' @export
zeros.ddf_col <- function(x, ...) {
  wrap_ddf_col(zeros)(x, ...)
}

#' @rdname basic-matrices-arrays
#' @export
zeros.tbl_ddf <- function(x, ...) {
  wrap_dibble(zeros)(x, ...)
}
