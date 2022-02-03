new_dibble_measure <- function(x, dim_names) {
  structure(x,
            dim_names = dim_names,
            class = "dibble_measure")
}

#' Build a dibble measure
#'
#' `dibble_measure()` constructs a measure for a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#'
#' @return A measure for a dibble.
#'
#' @export
dibble_measure <- function(x, dim_names = NULL) {
  if (is_dibble(x) || is_grouped_dibble(x)) {
    x <- as_dibble_measure(x)
    is_dibble_measure_x <- TRUE
  } else {
    is_dibble_measure_x <- is_dibble_measure(x)
  }

  if (is.null(dim_names)) {
    stopifnot(
      is_dibble_measure_x
    )
    as_dibble_measure(x)
  } else {
    broadcast(x, dim_names)
  }
}

#' Coerce an object to a dibble measure
#'
#' `as_dibble_measure()` turns an object into a measure for a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#' @param ... Unused, for extensibility.
#'
#' @return A measure for a dibble.
#'
#' @export
as_dibble_measure <- function(x, ...) {
  UseMethod("as_dibble_measure")
}

#' @rdname as_dibble_measure
#' @export
as_dibble_measure.dibble_measure <- function(x, ...) {
  x
}

#' @rdname as_dibble_measure
#' @export
as_dibble_measure.dibble <- function(x, ...) {
  stopifnot(
    ncol(x) == 1L
  )
  x[[1L]]
}

#' @rdname as_dibble_measure
#' @export
as_dibble_measure.grouped_dibble <- function(x, ...) {
  stopifnot(
    ncol(x) == 1L
  )
  x[[1L]]
}

#' Test if the object is a dibble measure
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble_measure <- function(x) {
  inherits(x, "dibble_measure")
}

#' @export
as.array.dibble_measure <- function(x, ...) {
  as.array(undibble(x))
}

#' @export
as.table.dibble_measure <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- undibble(x)
  dimnames(x) <- dim_names
  as.table(x)
}

#' @export
dimnames.dibble_measure <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.dibble_measure` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.dibble_measure <- function(x) {
  list_sizes(dimnames(x))
}

#' @export
nrow.dibble_measure <- function(x, ...) {
  nrow_dibble(x)
}

#' @export
ncol.dibble_measure <- function(x, ...) {
  NULL
}

#' @export
rownames.dibble_measure <- function(x, ...) {
  NULL
}

#' @export
colnames.dibble_measure <- function(x, ...) {
  NULL
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dibble_measure <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

#' @export
aperm.dibble_measure <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.dibble_measure <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dibble_measure <- function(x, ...) {
  x
}

#' @importFrom dplyr select
#' @export
select.dibble_measure <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.dibble_measure <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.dibble_measure <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.dibble_measure <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
