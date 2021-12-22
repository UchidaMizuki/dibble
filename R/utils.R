cat_line <- function(...) {
  cat(..., "\n",
      sep = "")
}

big_mark <- function(x) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark)
}

as_dim_names <- function(x) {
  x <- as.list(x)
  nms <- rlang::names2(x)

  names(x)[nms == ""] <- vapply(x[nms == ""],
                                function(x) {
                                  stopifnot(
                                    rlang::is_scalar_character(x)
                                  )
                                  x
                                },
                                FUN.VALUE = character(1))
  x[nms == ""] <- list(NULL)

  stopifnot(
    vapply(x[nms != ""],
           function(x) !vctrs::vec_duplicate_any(x),
           FUN.VALUE = logical(1))
  )
  x
}
