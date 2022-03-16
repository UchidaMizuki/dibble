commas <- function(...) {
  paste0(...,
         collapse = ", ")
}

big_mark <- function(x) {
  if (identical(getOption("OutDec"), ",")) {
    mark <- "."
  } else {
    mark <- ","
  }

  formatC(x,
          big.mark = mark,
          format = "d")
}

wrap_dibble <- function(f) {
  function(x, ...) {
    f(as_ddf_col(x), ...)
  }
}

wrap_ddf_col <- function(f) {
  function(x, ...) {
    dim_names <- dimnames(x)
    x <- f(as.array(x), ...)
    new_ddf_col(x, dim_names)
  }
}

list_sizes_unnamed <- function(x) {
  unname(list_sizes(x))
}
