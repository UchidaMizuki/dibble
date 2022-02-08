as_dim_names <- function(x, dim_names) {
  if (is.null(x)) {
    nms <- names(dim_names)
    x <- vec_init_along(list(), nms)
    names(x) <- nms
  } else if (is.character(x)) {
    nms <- x
    x <- vec_init_along(list(), x)
    names(x) <- nms
  } else {
    stopifnot(
      is.list(x)
    )

    loc <- rlang::names2(x) == ""
    nms <- vapply(x[loc],
                  function(x) {
                    stopifnot(
                      rlang::is_scalar_character(x)
                    )
                    x
                  },
                  character(1))
    x[loc] <- list(NULL)
    names(x)[loc] <- nms
  }

  stopifnot(
    rlang::is_named(x),
    vapply(x,
           function(x) {
             is.null(x) || !vec_duplicate_any(x)
           },
           logical(1L))
  )

  new_axes <- names(x)
  old_axes <- names(dim_names)
  stopifnot(
    old_axes %in% new_axes
  )
  dim_names <- dim_names[vec_match(new_axes, old_axes)]

  x <- mapply(x, dim_names,
              FUN = function(x, dim_name) {
                x <- x %||% vec_unique(dim_name)
                stopifnot(
                  !is.null(x)
                )
                x
              },
              SIMPLIFY = FALSE)
  names(x) <- new_axes
  x
}

union_dim_names <- function(...) {
  x <- vec_c(...)
  nms <- names(x)
  nms_unique <- unique(nms)
  out <- lapply(nms_unique,
                function(nm_unique) {
                  unique(vec_c(!!!unname(x[nms == nm_unique])))
                })
  names(out) <- nms_unique
  out
}
