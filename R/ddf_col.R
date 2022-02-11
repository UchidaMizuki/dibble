new_ddf_col <- function(x, dim_names) {
  structure(x,
            dim_names = dim_names,
            class = "ddf_col")
}

as_ddf_col <- function(x, ...) {
  UseMethod("as_ddf_col")
}

as_ddf_col.ddf_col <- function(x, ...) {
  x
}

as_ddf_col.tbl_ddf <- function(x, ...) {
  stopifnot(
    ncol(x) == 1L
  )

  x[[1L]]
}

as_ddf_col.grouped_ddf <- function(x, ...) {
  stopifnot(
    ncol(x) == 1L
  )

  x[[1L]]
}

as_ddf_col.array <- function(x, ...) {
  dim_names <- dimnames(x)
  stopifnot(
    !is.null(dim_names)
  )

  new_ddf_col(x, dim_names)
}

as_ddf_col.table <- function(x, ...) {
  as_ddf_col.array(x, ...)
}

is_ddf_col <- function(x) {
  inherits(x, "ddf_col")
}

#' @export
as.array.ddf_col <- function(x, ...) {
  as.array(undibble(x))
}

#' @export
as.table.ddf_col <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- undibble(x)
  dimnames(x) <- dim_names
  as.table(x)
}

#' @export
dimnames.ddf_col <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.ddf_col` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.ddf_col <- function(x) {
  dim_dibble(x)
}

#' @export
nrow.ddf_col <- function(x, ...) {
  nrow_dibble(x)
}

#' @export
ncol.ddf_col <- function(x, ...) {
  NULL
}

#' @export
rownames.ddf_col <- function(x, ...) {
  rownames_dibble(x, ...)
}

#' @export
colnames.ddf_col <- function(x, ...) {
  colnames_dibble(x, ...)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.ddf_col <- function(x, ...,
                              n = ".") {
  as_tibble_dibble(x, ...,
                   n = n)
}

#' @export
aperm.ddf_col <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.ddf_col <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr select
#' @export
select.ddf_col <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.ddf_col <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.ddf_col <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.ddf_col <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
