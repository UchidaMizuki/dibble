new_dim_col <- function(x, dim_names) {
  dim <- lengths(dim_names, use.names = FALSE)
  size <- prod(dim)
  structure(array(vctrs::vec_recycle(as.vector(x), size),
                  dim = dim),
            class = "dim_col",
            dim_names = dim_names)
}

is_dim_col <- function(x) {
  inherits(x, "dim_col")
}

#' @export
as.array.dim_col <- function(x, ...) {
  structure(x,
            class = NULL,
            dim_names = NULL)
}

#' @export
as.table.dim_col <- function(x, ...) {
  structure(as.array(x),
            class = "table",
            dimnames = dimnames(x))
}

#' @export
dimnames.dim_col <- function(x) {
  attr(x, "dim_names")
}

#' @export
`dimnames<-.dim_col` <- function(x, value) {
  attr(x, "dim_names") <- value
}

#' @export
dim.dim_col <- function(x) {
  lengths(dimnames(x))
}

#' @export
`dim<-.dim_col` <- function(x, value) {
  # TODO: add an error message
  stop()
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dim_col <- function(x, ...) {
  dim <- expand.grid(dimnames(x),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)

  tibble::tibble(dim = as_tibble(dim),
                 value = as.vector(x))
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.dim_col <- function(.data, ...) {
  slice_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.dim_col <- function(x, n = NULL, ...) {
  out <- head_dibble(x, n)
  out <- vctrs::new_data_frame(as_tibble(out),
                               class = c("tbl_dim_impl", "tbl"))
  attr(out, "tbl_sum") <- c(`A dimensional column` = obj_sum(x))
  attr(out, "rows_total") <- prod(dim(x))
  print(out)

  invisible(x)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.dim_col <- function(x) {
  paste0("dim", pillar::size_sum(x))
}
