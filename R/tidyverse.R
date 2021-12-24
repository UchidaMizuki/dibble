as_tibble_dibble <- function(x, ..., .pack = FALSE) {
  dim <- expand.grid(dimnames(x),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  dim <- as_tibble(dim)

  if (is_tbl_dim(x)) {
    col <- purrr::map_dfc(as.list(x), as.vector)

    if (.pack) {
      tibble::new_tibble(list(dim = dim,
                              col = col))
    } else {
      vctrs::vec_cbind(dim, col, ...)
    }
  } else if (is_dim_col(x)) {
    tibble::new_tibble(list(dim = dim,
                            . = as.vector(as.array(x))))
  }
}

slice_dibble <- function(.data, ...) {
  dots <- purrr::modify(rlang::list2(...),
                        function(x) {
                          x %||% rlang::missing_arg()
                        })
  nms <- rlang::names2(dots)

  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  stopifnot(
    length(dots) == length(dim_names),
    nms == "" | nms %in% axes
  )

  names(dots)[nms == ""] <- axes[!axes %in% nms]
  dots <- dots[axes]
  dim_names <- purrr::modify2(dim_names, dots, `[`)
  names(dim_names) <- axes

  if (is_tbl_dim(.data)) {
    new_tbl_dim(purrr::modify(.data, function(x) rlang::exec(`[`, x, !!!dots)),
                dim_names = dim_names)
  } else if (is_dim_col(.data)) {
    new_dim_col(rlang::exec(`[`, .data, !!!dots),
                dim_names = dim_names)
  }
}
