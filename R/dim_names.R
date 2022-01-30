as_dim_names <- function(x, data) {
  if (is.null(x)) {
    nms <- names(data)
    x <- vec_init_along(list(), nms)
    names(x) <- nms
  }

  stopifnot(
    rlang::is_named(x),
    vapply(x,
           function(x) {
             is.null(x) || !vec_duplicate_any(x)
           },
           logical(1L))
  )

  axes <- names(x)
  x <- mapply(x, axes,
              FUN = function(x, axis) {
                x <- x %||% vec_unique(data[[axis]])
                stopifnot(
                  !is.null(x)
                )
                x
              },
              SIMPLIFY = FALSE)
  names(x) <- axes
  x
}

union_dim_names <- function(...) {
  x <- vec_c(...)
  nms <- names(x)
  tapply(x, factor(nms, vec_unique(nms)),
         function(x) {
           unique(vec_c(!!!unname(x)))
         },
         simplify = FALSE)
}
