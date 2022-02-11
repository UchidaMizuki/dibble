new_tbl_ddf <- function(x, dim_names) {
  structure(x,
            dim_names = dim_names,
            class = "tbl_ddf")
}

is_tbl_ddf <- function(x) {
  inherits(x, "tbl_ddf")
}

#' @export
as.list.tbl_ddf <- function(x, ...) {
  dim_names <- dimnames(x)
  lapply(undibble(x),
         function(x) {
           new_ddf_col(x, dim_names)
         })
}

#' @export
as.array.tbl_ddf <- function(x, ...) {
  as.array(as_ddf_col(x), ...)
}

#' @export
as.table.tbl_ddf <- function(x, ...) {
  as.table(as_ddf_col(x), ...)
}

#' @export
dimnames.tbl_ddf <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.tbl_ddf` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.tbl_ddf <- function(x) {
  dim_dibble(x)
}

#' The number of rows/columns
#'
#' `nrow()` and `ncol()` return the number of rows or columns present in x.
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
nrow.tbl_ddf <- function(x, ...) {
  nrow_dibble(x, ...)
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
ncol.tbl_ddf <- function(x, ...) {
  ncol_dibble(x, ...)
}

#' Row and column names
#'
#' Retrieve or set the row or column names of a matrix-like object.
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
rownames.tbl_ddf <- function(x, ...) {
  rownames_dibble(x, ...)
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
colnames.tbl_ddf <- function(x, ...) {
  colnames_dibble(x, ...)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.tbl_ddf <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

#' @export
aperm.tbl_ddf <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.tbl_ddf` <- function(x, i) {
  new_tbl_ddf(NextMethod(), dimnames(x))
}

#' @export
`[[.tbl_ddf` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}

#' @export
`$.tbl_ddf` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.tbl_ddf <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr mutate
#' @export
mutate.tbl_ddf <- function(.data, ...) {
  dots <- enquos(...,
                 .named = TRUE)
  nms <- names(dots)

  dim_names <- dimnames(.data)
  data <- as.list(.data)

  .data <- undibble(.data)

  for (i in vec_seq_along(nms)) {
    nm <- nms[[i]]

    data_nm <- broadcast(eval_tidy(dots[[i]], data),
                         dim_names = dim_names)

    data[[nm]] <- data_nm
    .data[[nm]] <- undibble(data_nm)
  }
  new_tbl_ddf(.data, dim_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_ddf <- function(x, ...) {
  x
}

#' @importFrom dplyr select
#' @export
select.tbl_ddf <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.tbl_ddf <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.tbl_ddf <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.tbl_ddf <- function(x, n = NULL, ...) {
  print_dibble(x, n, ...)
}
