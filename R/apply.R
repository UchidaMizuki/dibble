#' Apply functions over array margins
#'
#' Applying a function to margins of a dibble or array, including a matrix.
#'
#' `apply()` overrides [base::apply()] to make it generic. The default method
#' calls the base version.
#'
#' @param X A dibble or array, including a matrix.
#' @param MARGIN An integer or character vector giving the subscripts which the
#' function will be applied over.
#' @param FUN A function to be applied.
#' @param ... Optional arguments to FUN.
#'
#' @param simplify A logical indicating whether results should be simplified if
#' possible.
#'
#' @return A dibble if X is a dibble. See [base::apply()] for the return value
#' of the default method.
#'
#' @seealso [base::apply()].
#'
#' @examples
#' x <- array(1:24, 2:4,
#'            list(axis1 = letters[1:2],
#'                 axis2 = letters[1:3],
#'                 axis3 = letters[1:4]))
#'
#' apply(x, 2:3, sum)
#' apply(as_dibble(x), 2:3, sum)
#'
#' apply(x, c("axis2", "axis3"), sum)
#' apply(as_dibble(x), c("axis2", "axis3"), sum)
#'
#' @export
apply <- function(X, MARGIN, FUN, ...) {
  UseMethod("apply")
}

#' @rdname apply
#' @export
apply.default <- function(X, MARGIN, FUN, ...,
                          simplify = TRUE) {
  base::apply(X, MARGIN, FUN, ...,
              simplify = simplify)
}

#' @rdname apply
#' @export
apply.tbl_ddf <- function(X, MARGIN, FUN, ...) {
  apply_dibble(X, MARGIN, FUN, ...)
}

#' @rdname apply
#' @export
apply.grouped_ddf <- function(X, MARGIN, FUN, ...) {
  apply_dibble(X, MARGIN, FUN, ...)
}

apply_dibble <- function(X, MARGIN, FUN, ...) {
  apply(as_ddf_col(X), MARGIN, FUN, ...)
}

#' @rdname apply
#' @export
apply.ddf_col <- function(X, MARGIN, FUN, ...) {
  dim_names <- dimnames(X)

  if (is.character(MARGIN)) {
    MARGIN <- vec_match(MARGIN, names(dim_names))
  }

  X <- apply(as.array(X), MARGIN, FUN, ...)
  new_ddf_col(X, dim_names[MARGIN])
}
