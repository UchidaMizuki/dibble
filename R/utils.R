commas <- function(...) {
  paste0(...,
         collapse = ", ")
}

big_mark <- function(x) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark)
}

as_dim_names <- function(x) {
  x <- as.list(x)
  nms <- rlang::names2(x)

  names(x)[nms == ""] <- purrr::map_chr(x[nms == ""], identity)
  x[nms == ""] <- list(NULL)

  stopifnot(
    purrr::map_lgl(x[nms != ""],
                   function(x) !vctrs::vec_duplicate_any(x))
  )

  x
}

bind_arrays <- function(x) {
  perm <- length(dim(x[[1]]))
  x <- abind::abind(x,
                    rev.along = 0)
  aperm(x,
        perm = c(perm + 1, seq_len(perm)))
}
