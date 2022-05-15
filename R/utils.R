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

set_intersect <- function(x, y) {
  loc_in_y <- vec_match(x, y)
  loc_in_y <- vec_slice(loc_in_y, !is.na(loc_in_y))
  vec_slice(y, loc_in_y)
}

set_diff <- function(x, y) {
  x_in_y <- vec_in(x, y)
  vec_slice(x, !x_in_y)
}
