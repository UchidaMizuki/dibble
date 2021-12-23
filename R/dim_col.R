new_dim_col <- function(x, dim_names) {
  dim <- lengths(dim_names,
                 use.names = FALSE)
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



# Printing ----------------------------------------------------------------

#' @export
print.dim_col <- function(x, n = NULL, ...) {
  x_head <- head_dibble(x, n)
  df <- as_tibble_dibble(x_head,
                         pack = TRUE)
  df <- vctrs::new_data_frame(df,
                              class = c("tbl_dim_impl", "tbl"))
  attr(df, "tbl_sum") <- c(`A dimensional column` = obj_sum(x))
  attr(df, "rows_total") <- prod(dim(x))
  print(df)

  invisible(x)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.dim_col <- function(x) {
  paste("dim", pillar::size_sum(x))
}
