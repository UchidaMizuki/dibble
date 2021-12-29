as_tibble_dibble <- function(x, ..., .pack = FALSE) {
  dm <- tidyr::expand_grid(!!!dimnames(x))

  if (is_tbl_dim(x)) {
    col <- purrr::map_dfc(as.list(x), as_col)

    if (.pack) {
      tibble::new_tibble(list(dim = dm,
                              col = col))
    } else {
      vctrs::vec_cbind(dm, col, ...)
    }
  } else if (is_dim_col(x)) {
    col <- as_col(x)

    if (.pack) {
      tibble::new_tibble(list(dim = dm,
                              . = col))
    } else {
      vctrs::vec_cbind(dm,
                       . = col, ...)
    }
  }
}

as_col <- function(x) {
  as.vector(aperm(as.array(x)))
}

slice_dibble <- function(.data, ...) {
  loc <- purrr::modify(rlang::list2(...),
                       function(x) {
                         x %||% rlang::missing_arg()
                       })
  nms <- rlang::names2(loc)

  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  stopifnot(
    length(loc) == length(dim_names),
    nms == "" | nms %in% axes
  )

  names(loc)[nms == ""] <- axes[!axes %in% nms]
  loc <- loc[axes]
  dim_names <- purrr::modify2(dim_names, loc, `[`)
  names(dim_names) <- axes

  if (is_dim_col(.data)) {
    env_dibble <- environment_dibble(dim_names)
    new_dim_col(rlang::exec(`[`, .data, !!!loc,
                            drop = FALSE),
                environment = env_dibble)
  } else if (is_tbl_dim(.data)) {
    env_dibble <- environment_dibble(dim_names)
    new_tbl_dim(purrr::modify(as.list(.data),
                              function(x) {
                                slice(x, !!!loc)
                              }),
                environment = env_dibble)
  } else if (is_grouped_dim(.data)) {
    groups <- seq_along(dim(as.array(.data)))

    .data <- rlang::exec(`[`, .data, !!!loc[groups],
                         drop = FALSE)
    purrr::modify(.data,
                  function(x) {
                    slice(x, !!!loc[-groups])
                  })
  }
}
