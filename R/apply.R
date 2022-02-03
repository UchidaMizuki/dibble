#' @export
apply <- function(X, MARGIN, FUN, ...) {
  UseMethod("apply")
}

#' @export
apply.default <- function(X, MARGIN, FUN, ...,
                          simplify = TRUE) {
  base::apply(X, MARGIN, FUN, ...,
              simplify = simplify)
}

#' @export
apply.dibble <- function(X, MARGIN, FUN, ...) {
  apply_dibble(X, MARGIN, FUN, ...)
}

#' @export
apply.grouped_dibble <- function(X, MARGIN, FUN, ...) {
  apply_dibble(X, MARGIN, FUN, ...)
}

apply_dibble <- function(X, MARGIN, FUN, ...) {
  X <- as_dibble_measure(X)
  apply(X, MARGIN, FUN, ...)
}

#' @export
apply.dibble_measure <- function(X, MARGIN, FUN, ...) {
  dim_names <- dimnames(X)

  if (is.character(MARGIN)) {
    MARGIN <- vec_match(MARGIN, names(dim_names))
  }

  X <- apply(as.array(X), MARGIN, FUN, ...)

  # FIXME?: Dealing with the case where FUN returns a vector.
  new_dibble_measure(X, dim_names[MARGIN])
}
