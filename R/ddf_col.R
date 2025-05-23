new_ddf_col <- function(x, dim_names, class = character()) {
  structure(
    x,
    dim_names = dim_names,
    class = c(setdiff(class, "ddf_col"), "ddf_col")
  )
}

as_ddf_col <- function(x, ...) {
  if (is_tbl_ddf(x)) {
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
as.matrix.ddf_col <- function(x, ...) {
  if (vec_size(dimnames(x)) > 2) {
    abort("The dimension of `x` must be 1 or 2.")
  }

  as.matrix(undibble(x))
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
as_tibble.ddf_col <- function(x, ..., n = ".") {
  as_tibble_dibble(x, n)
}

#' @export
as.data.frame.ddf_col <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(as_tibble(x, ...), row.names = row.names, optional = optional)
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
  select_dibble(.data, ..., .relocate = TRUE)
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
  print_dibble(x, n = n, ...)
}

#' @export
format.ddf_col <- function(x, n = NULL, ...) {
  format_dibble(x, n = n, ...)
}

#' @export
tbl_sum.ddf_col <- function(x) {
  dim_names <- dimnames(x)
  dim <- list_sizes_unnamed(dim_names)
  size_dim <- prod(dim)

  c(
    `A dibble` = big_mark(size_dim),
    `Dimensions` = commas(paste0(names(dim_names), " [", big_mark(dim), "]"))
  )
}

#' @export
tbl_format_setup.ddf_col <- function(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL,
  focus = NULL
) {
  tbl_format_setup_dibble(
    x,
    width = width,
    ...,
    n = n,
    max_extra_cols = max_extra_cols,
    max_footer_lines = max_footer_lines,
    focus = focus
  )
}

#' @export
tbl_format_header.ddf_col <- function(x, setup, ...) {
  tbl_format_header_dibble(x, setup, ...)
}

#' @export
tbl_format_body.ddf_col <- function(x, setup, ...) {
  tbl_format_body_dibble(x, setup, ...)
}

#' @export
tbl_format_footer.ddf_col <- function(x, setup, ...) {
  tbl_format_footer_dibble(x, setup, ...)
}
