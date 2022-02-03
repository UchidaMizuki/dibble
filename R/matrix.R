#' @export
solve.dibble_measure <- function(a, b, ...) {
  if (is_missing(b)) {
    dim_names <- dimnames(a)
    a <- undibble(a)
    new_dibble_measure(unname(solve(a)), dim_names)
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
  ellipsis::check_dots_empty()

  if (is.matrix(x)) {
    base::diag(x,
               names = names, ...)
  } else {
    base::diag(x, nrow, ncol, names)
  }
}

#' @export
diag.dibble <- function(x, axes, ...) {
  diag_dibble(x, axes, ...)
}

#' @export
diag.grouped_dibble <- function(x, axes, ...) {
  diag_dibble(x, axes, ...)
}

diag_dibble <- function(x, axes, ...) {
  diag(as_dibble_measure(x), axes, ...)
}

#' @export
diag.dibble_measure <- function(x, axes, ...) {
  ellipsis::check_dots_empty()

  old_dim_names <- dimnames(x)
  is_scalar_old_dim_names <- rlang::is_scalar_list(old_dim_names)
  stopifnot(
    is_scalar_old_dim_names || vec_size(old_dim_names) == 2L
  )

  if (is_scalar_old_dim_names) {
    stopifnot(
      vec_size(axes) == 2L
    )

    new_dim_names <- vec_c(old_dim_names, old_dim_names)
    names(new_dim_names) <- axes
  } else {
    stopifnot(
      rlang::is_scalar_character(axes),
      identical(old_dim_names[[1L]], old_dim_names[[2L]])
    )

    new_dim_names <- old_dim_names[1L]
    names(new_dim_names) <- axes
  }
  new_dibble_measure(diag(as.array(x), ...),
                     new_dim_names)
}
