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
