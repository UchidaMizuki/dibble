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

    loc <- names2(x) == ""
    nms <- purrr::map_chr(x[loc], function(x) {
      stopifnot(
        is_scalar_character(x)
      )
      x
    })
    x[loc] <- list(NULL)
    names(x)[loc] <- nms
  }

  stopifnot(
    is_named(x),
    purrr::every(x, function(x) {
      is.null(x) || !vec_duplicate_any(x)
    })
  )

  new_axes <- names(x)
  old_axes <- names(dim_names)
  stopifnot(
    old_axes %in% new_axes
  )

  dim_names <- dim_names[vec_match(new_axes, old_axes)]

  x <- purrr::map2(x, dim_names, function(x, dim_name) {
    x <- x %||% unique(dim_name)
    stopifnot(
      !is.null(x)
    )
    x
  })
  names(x) <- new_axes
  x
}

is_dim_names <- function(x) {
  is_named(x)
}

union_dim_names <- function(x) {
  x <- list_unchop(x)
  nms <- names(x)
  nms_unique <- unique(nms)
  out <- purrr::map(nms_unique, function(nm_unique) {
    unique(list_unchop(unname(x[nms == nm_unique])))
  })
  names(out) <- nms_unique
  out
}

intersect_dim_names <- function(x) {
  names <- purrr::map(x, names)
  common_names <- purrr::reduce(names, set_intersect)

  tr <- purrr::list_transpose(x, simplify = FALSE, template = common_names)
  purrr::modify(tr, function(x) purrr::reduce(x, set_intersect))
}

diff_dim_names <- function(x) {
  purrr::modify(purrr::list_transpose(x, simplify = FALSE), function(x) {
    purrr::reduce(x, set_diff)
  })
}
