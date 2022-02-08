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
apply.tbl_ddf <- function(X, MARGIN, FUN, ...) {
  apply_dibble(X, MARGIN, FUN, ...)
}

#' @export
apply.grouped_ddf <- function(X, MARGIN, FUN, ...) {
  apply_dibble(X, MARGIN, FUN, ...)
}

apply_dibble <- function(X, MARGIN, FUN, ...) {
  X <- as_ddf_col(X)
  apply(X, MARGIN, FUN, ...)
}

#' @export
apply.ddf_col <- function(X, MARGIN, FUN, ...) {
  dim_names <- dimnames(X)

  if (is.character(MARGIN)) {
    MARGIN <- vec_match(MARGIN, names(dim_names))
  }

  X <- apply(as.array(X), MARGIN, FUN, ...)

  # FIXME?: Dealing with the case where FUN returns a vector.
  new_ddf_col(X, dim_names[MARGIN])
}
