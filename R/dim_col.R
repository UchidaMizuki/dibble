new_dim_col <- function(x, dim_names) {
  structure(x,
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
nrow.dim_col <- function(x) {
  nrow_dibble(x)
}

#' @export
ncol.dim_col <- function(x) {
  1L
}



# Printing ----------------------------------------------------------------

#' @export
print.dim_col <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.dim_col <- function(x) {
  paste("dim", pillar::size_sum(x))
}
