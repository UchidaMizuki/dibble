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

as_dim_names <- function(x, data) {
  if (is.null(x)) {
    x <- rep_along(data, list(NULL))
    names(x) <- names(data)
  } else {
    stopifnot(
      is_named(x),
      purrr::map_lgl(x,
                     function(x) {
                       is.null(x) || !vctrs::vec_duplicate_any(x)
                     })
    )
  }

  axes <- names(x)
  x <- purrr::modify2(x, axes,
                      function(x, axis) {
                        x <- x %||% unique(data[[axis]])

                        stopifnot(
                          !is.null(x)
                        )

                        x
                      })
  names(x) <- axes
  x
}

expand_dim_names <- function(x) {
  x <- purrr::flatten(x)
  nms <- names(x)
  axes <- unique(nms)
  x <- tapply(x, nms,
              function(x) {
                unique(unlist(x))
              },
              simplify = FALSE)
  x[axes]
}

bind_arrays <- function(x) {
  perm <- length(dim(x[[1]]))
  x <- abind::abind(x,
                    rev.along = 0)
  aperm(x, c(perm + 1, seq_len(perm)))
}

dim_sum <- function(x) {
  dim <- paste0(big_mark(dim(x)),
                collapse = " x ")
  paste0("dim [", dim, "]")
}
