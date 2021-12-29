commas <- function(...) {
  paste0(...,
         collapse = ", ")
}

big_mark <- function(x) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark)
}

as_dim_names <- function(x, data) {
  stopifnot(
    rlang::is_named(x),
    purrr::map_lgl(x,
                   function(x) {
                     is.null(x) || !vctrs::vec_duplicate_any(x)
                   })
  )

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

bind_arrays <- function(x) {
  perm <- length(dim(x[[1]]))
  x <- abind::abind(x,
                    rev.along = 0)
  aperm(x,
        perm = c(perm + 1, seq_len(perm)))
}

environment_dibble <- function(dim_names) {
  rlang::as_environment(list(dim_names = dim_names))
}
