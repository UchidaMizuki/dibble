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
