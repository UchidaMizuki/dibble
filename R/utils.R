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
                dim_names = dimnames(x),
                class = class(x))
  }
}

list_sizes_unnamed <- function(x) {
  unname(list_sizes(x))
}

expand_grid_col_major <- function(x) {
  rev(tidyr::expand_grid(!!!rev(x)))
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

all_equal_dim_names <- function(target, current) {
  if (identical(names(target), names(current))) {
    all(purrr::map2_lgl(target, current,
                        function(target, current) {
                          vec_size(target) == vec_size(current) &&
                            all(target == current)
                        }))
  } else {
    FALSE
  }
}

# From pillar:::get_n_print()
get_n_print <- function(n, rows) {
  if (!is.null(n) && n >= 0) {
    return(n)
  }
  if (is.na(rows) || rows > 20) {
    10
  }
  else {
    rows
  }
}
