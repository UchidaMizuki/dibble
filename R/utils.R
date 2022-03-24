commas <- function(...) {
  paste0(...,
         collapse = ", ")
}

big_mark <- function(x) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x,
          big.mark = mark,
          format = "d")
}

wrap_dibble <- function(f) {
  function(x, ...) {
    f(as_ddf_col(x), ...)
  }
}

wrap_ddf_col <- function(f, matrix = FALSE) {
  if (matrix) {
    as <- as.matrix
  } else {
    as <- as.array
  }

  function(x, ...) {
    new_ddf_col(f(as(x), ...),
                dim_names = dimnames(x))
  }
}

list_sizes_unnamed <- function(x) {
  unname(list_sizes(x))
}

expand_grid_col_major <- function(...) {
  rev(tidyr::expand_grid(!!!rev(list2(...))))
}
