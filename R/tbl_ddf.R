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
  purrr::modify(undibble(x),
                function(x) {
                  new_ddf_col(x, dim_names)
                })
}

#' @export
as.array.tbl_ddf <- function(x, ...) {
  wrap_dibble(as.array)(x, ...)
}

#' @export
as.matrix.tbl_ddf <- function(x, ...) {
  wrap_dibble(as.matrix)(x, ...)
}

#' @export
as.table.tbl_ddf <- function(x, ...) {
  wrap_dibble(as.table)(x, ...)
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

#' @importFrom tibble as_tibble
#' @export
as_tibble.tbl_ddf <- function(x, ...,
                              n = NULL) {
  as_tibble_dibble(x, n)
}

#' @export
aperm.tbl_ddf <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}

#' @export
`!.tbl_ddf` <- function(x) {
  wrap_dibble(`!`)(x)
}

#' @export
is.finite.tbl_ddf <- function(x) {
  wrap_dibble(is.finite)(x)
}

#' @export
is.infinite.tbl_ddf <- function(x) {
  wrap_dibble(is.infinite)(x)
}

#' @export
is.na.tbl_ddf <- function(x) {
  wrap_dibble(is.na)(x)
}

#' @export
is.nan.tbl_ddf <- function(x) {
  wrap_dibble(is.nan)(x)
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

    data_nm <- suppress_warning_broadcast(
      broadcast(eval_tidy(dots[[i]], data),
                dim_names = dim_names)
    )

    data[[nm]] <- data_nm
    .data[[nm]] <- undibble(data_nm)
  }
  new_tbl_ddf(.data, dim_names)
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

#' @importFrom dplyr filter
#' @export
filter.tbl_ddf <- function(.data, ..., .preserve = FALSE) {
  filter_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.tbl_ddf <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
