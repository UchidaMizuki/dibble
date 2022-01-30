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

# bind_arrays <- function(x) {
#   perm <- length(dim(x[[1]]))
#   x <- abind::abind(x,
#                     rev.along = 0)
#   aperm(x, c(perm + 1, seq_len(perm)))
# }
