new_ddf_col <- function(x, dim_names) {
  structure(x,
            dim_names = dim_names,
            class = "ddf_col")
}

as_ddf_col <- function(x, ...) {
  if (is_tbl_ddf(x) || is_grouped_ddf(x)) {
    stopifnot(
      ncol(x) == 1L
    )

    x <- x[[1L]]
  }
  x
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

#' @importFrom tibble as_tibble
#' @export
as_tibble.ddf_col <- function(x, ...,
                              n = ".") {
  as_tibble_dibble(x, n)
}

#' @export
aperm.ddf_col <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}

#' @export
`!.ddf_col` <- function(x) {
  wrap_ddf_col(`!`)(x)
}

#' @export
is.finite.ddf_col <- function(x) {
  wrap_ddf_col(is.finite)(x)
}

#' @export
is.infinite.ddf_col <- function(x) {
  wrap_ddf_col(is.infinite)(x)
}

#' @export
is.na.ddf_col <- function(x) {
  wrap_ddf_col(is.na)(x)
}

#' @export
is.nan.ddf_col <- function(x) {
  wrap_ddf_col(is.nan)(x)
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

#' @importFrom dplyr filter
#' @export
filter.ddf_col <- function(.data, ..., .preserve = FALSE) {
  filter_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.ddf_col <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
