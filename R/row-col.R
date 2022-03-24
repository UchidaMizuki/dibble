#' The number of rows/columns
#'
#' `nrow()` and `ncol()` return the number of rows or columns present in x.
#'
#' These functions override base functions to make them generic. The default
#' methods call the base versions.
#'
#' @param x An object.
#' @param ... Other arguments passed on to methods.
#'
#' @return An integer or `NULL`.
#'
#' @name nrow-ncol
NULL

#' @rdname nrow-ncol
#' @export
nrow <- function(x, ...) {
  UseMethod("nrow")
}

#' @rdname nrow-ncol
#' @export
nrow.default <- function(x, ...) {
  base::nrow(x)
}

#' @rdname nrow-ncol
#' @export
nrow.ddf_col <- function(x, ...) {
  nrow_dibble(x)
}

#' @rdname nrow-ncol
#' @export
nrow.tbl_ddf <- function(x, ...) {
  nrow_dibble(x, ...)
}

nrow_dibble <- function(x, ...) {
  prod(dim(x))
}

#' @rdname nrow-ncol
#' @export
ncol <- function(x, ...) {
  UseMethod("ncol")
}

#' @rdname nrow-ncol
#' @export
ncol.default <- function(x, ...) {
  base::ncol(x)
}

#' @rdname nrow-ncol
#' @export
ncol.ddf_col <- function(x, ...) {
  NULL
}

#' @rdname nrow-ncol
#' @export
ncol.tbl_ddf <- function(x, ...) {
  ncol_dibble(x, ...)
}

ncol_dibble <- function(x, ...) {
  if (is_ddf_col(x)) {
    NULL
  } else {
    vec_size(colnames(x))
  }
}

#' Row and column names
#'
#' Retrieve or set the row or column names of a matrix-like object.
#'
#' These functions override base functions to make them generic. The default
#' methods call the base versions.
#'
#' @param x A matrix-like object.
#' @param ... Other arguments passed on to methods.
#'
#' @return A list of row/column names.
#'
#' @name row-colnames
NULL

#' @rdname row-colnames
#' @export
rownames <- function(x, ...) {
  UseMethod("rownames")
}

#' @rdname row-colnames
#' @export
rownames.default <- function(x, ...) {
  base::rownames(x, ...)
}

#' @rdname row-colnames
#' @export
rownames.ddf_col <- function(x, ...) {
  rownames_dibble(x, ...)
}

#' @rdname row-colnames
#' @export
rownames.tbl_ddf <- function(x, ...) {
  rownames_dibble(x, ...)
}

rownames_dibble <- function(x, ...) {
  NULL
}

#' @rdname row-colnames
#' @export
colnames <- function(x, ...) {
  UseMethod("colnames")
}

#' @rdname row-colnames
#' @export
colnames.default <- function(x, ...) {
  base::colnames(x, ...)
}

#' @rdname row-colnames
#' @export
colnames.ddf_col <- function(x, ...) {
  colnames_dibble(x, ...)
}

#' @rdname row-colnames
#' @export
colnames.tbl_ddf <- function(x, ...) {
  colnames_dibble(x, ...)
}

colnames_dibble <- function(x, ...) {
  if (is_ddf_col(x)) {
    NULL
  } else {
    names(x)
  }
}
