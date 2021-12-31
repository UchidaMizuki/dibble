as_tibble_dibble <- function(x, ...) {
  dim <- tidyr::expand_grid(!!!dimnames(x))

  if (is_grouped_dibble(x)) {
    x <- ungroup(x)
  }

  if (is_dibble(x)) {
    meas <- purrr::map_dfc(as_list_dibble(x), as_meas)
    vctrs::vec_cbind(dim, meas, ...)
  } else if (is_dibble_measure(x)) {
    vctrs::vec_cbind(dim,
                     . = as_meas(x), ...)
  }
}

as_meas <- function(x) {
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

  if (is_dibble_measure(.data)) {
    new_dibble_measure(rlang::exec(`[`, .data, !!!loc,
                                   drop = FALSE),
                       dim_names = dim_names)
  } else if (is_dibble(.data)) {
    new_dibble(purrr::modify(as.list(.data),
                             function(x) {
                               as.array(slice(x, !!!loc))
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

    .data <- purrr::modify(as_list_dibble(.data),
                           function(x) {
                             x <- rlang::exec(`[`, x, !!!loc_groups,
                                              drop = FALSE)
                             purrr::modify(x,
                                           function(x) {
                                             slice(x, !!!loc)
                                           })
                           })
    new_grouped_dibble(.data, group_names)
  }
}

select_dibble <- function(.data, ...) {
  loc <- tidyselect::eval_select(expr(c(...)), as_list_dibble(.data))
  .data[loc]
}

# rename_dibble <- function(.data, ...) {
#   tidyselect::
# }
