as_tibble_dibble <- function(x, ...) {
  dim <- exec(tidyr::expand_grid, !!!dimnames(x))

  if (is_grouped_dibble(x)) {
    x <- ungroup(x)
  }

  if (is_dibble(x)) {
    meas <- purrr::modify(undibble(x), as_meas)
    vctrs::vec_cbind(dim, !!!meas, ...,
                     .name_repair = "check_unique")
  } else if (is_dibble_metric(x)) {
    vctrs::vec_cbind(dim,
                     . = as_meas(x), ...,
                     .name_repair = "check_unique")
  }
}

as_meas <- function(x) {
  as.vector(aperm(as.array(x)))
}

slice_dibble <- function(.data, ...) {
  loc <- purrr::modify(list2(...),
                       function(x) {
                         x %||% missing_arg()
                       })
  nms <- names2(loc)

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

  if (is_dibble_metric(.data)) {
    new_dibble_metric(exec(`[`, .data, !!!loc,
                            drop = FALSE),
                       dim_names = dim_names)
  } else if (is_dibble(.data)) {
    new_dibble(purrr::modify(undibble(.data),
                             function(x) {
                               exec(`[`, x, !!!loc,
                                    drop = FALSE)
                             }),
               dim_names = dim_names)
  } else if (is_grouped_dibble(.data)) {
    group_names <- attr(.data, "group_names")
    group_axes <- names(group_names)
    groups <- seq_along(group_names)

    loc_groups <- loc[groups]
    loc <- loc[-groups]

    group_names <- purrr::modify2(group_names, loc_groups,
                                  function(x, i) {
                                    x[i]
                                  })
    names(group_names) <- group_axes

    .data <- purrr::modify(undibble(.data),
                           function(x) {
                             x <- exec(`[`, x, !!!loc_groups,
                                       drop = FALSE)
                             purrr::modify(x,
                                           function(x) {
                                             slice(x, !!!loc)
                                           })
                           })
    new_grouped_dibble(.data, group_names)
  }
}

select_dibble <- function(.data, ..., .relocate = FALSE) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  group_names <- group_names(.data)
  group_axes <- names(group_names)

  if (is_dibble(.data) || is_grouped_dibble(.data)) {
    data <- c(dim_names, .data)
  } else if (is_dibble_metric(.data)) {
    data <- dim_names
  }

  nms <- names(tidyselect::eval_select(expr(c(...)), data))

  if (is_dibble(.data) || is_grouped_dibble(.data)) {
    if (.relocate) {
      meas_names <- colnames(.data)
      .data <- .data[perm_match(nms, meas_names)]
    } else {
      .data <- .data[setdiff(nms, axes)]
    }
  }

  if (is_grouped_dibble(.data)) {
    perm_groups <- perm_match(nms, group_axes)
    axes <- setdiff(axes, group_axes)
    perm <- perm_match(nms, axes)

    group_names <- group_names[perm_groups]
    .data <- purrr::modify(undibble(.data),
                           function(x) {
                             x <- aperm(x, perm_groups)
                             purrr::modify(x,
                                           function(x) {
                                             aperm(x, perm)
                                           })
                           })
    new_grouped_dibble(.data, group_names)

  } else if (is_dibble(.data) || is_dibble_metric(.data)) {
    perm <- perm_match(nms, axes)
    aperm(.data, perm)
  }
}

perm_match <- function(x, y) {
  vec_match(c(intersect(x, y), setdiff(y, x)), y)
}

rename_dibble <- function(.data, ...) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  group_names <- group_names(.data)
  group_axes <- names(group_names)

  if (is_dibble(.data) || is_grouped_dibble(.data)) {
    data <- c(dim_names, .data)
  } else if (is_dibble_metric(.data)) {
    data <- dim_names
  }

  nms <- names(data)
  names(nms) <- nms
  loc <- tidyselect::eval_rename(expr(c(...)), data)
  nms[loc] <- names(loc)

  names(dimnames(.data)) <- nms[axes]

  if (is_dibble(.data) || is_grouped_dibble(.data)) {
    meas_names <- colnames(.data)
    names(.data) <- nms[meas_names]
  }
  .data
}
