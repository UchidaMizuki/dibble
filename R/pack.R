#' Pack and unpack dibble
#'
#' @param ... Passed to `tidyr::pack()` or `tidyr::unpack()`.
#' @param data,.data A dibble
#' @param cols Passed to `tidyr::unpack()`.
#' @param names_sep,.names_sep Passed to `tidyr::pack()` or `tidyr::unpack()`.
#'
#' @export
dibble_pack <- function(.data, ...,
                        .names_sep = NULL) {
  args <- enquos(...)

  for (i in seq_along(args)) {
    dim_names <- dimnames(.data)
    loc <- tidyselect::eval_select(args[[i]], dim_names)

    perm <- names(dim_names)
    perm <- append(perm[-loc],
                   names(loc),
                   after = loc[[1]] - 1)

    dim_names_new <- expand_grid_col_major(dim_names[loc])
    dim_names_new <- tidyr::pack(dim_names_new, !!!args[i],
                                 .names_sep = .names_sep)
    names(dim_names)[[loc[[1]]]] <- names(dim_names_new)
    dim_names[[loc[[1]]]] <- dim_names_new[[1]]
    dim_names[loc[-1]] <- NULL
    dim <- list_sizes(dim_names)

    .data <- aperm(.data, perm)

    if (is_ddf_col(.data)) {
      dim(.data) <- dim
    } else {
      for (i in length(.data)) {
        data_i <- as.array(.data[[i]])
        dim(data_i) <- dim
        .data[[i]] <- data_i
      }
    }
    attr(.data, "dim_names") <- dim_names
  }
  .data
}

#' @rdname dibble_pack
#' @export
dibble_unpack <- function(data, cols, ...,
                          names_sep = NULL) {
  dim_names <- dimnames(data)
  names <- names(tidyselect::eval_select(rlang::enquo(cols), dim_names))

  for (i in seq_along(names)) {
    dim_names_new <- as.list(dim_names[[names[[i]]]]) |>
      purrr::map(vec_unique)
    dim_names[[names[[i]]]] <- expand_grid_col_major(dim_names_new) |>
      tibble::as_tibble()

    data <- broadcast(data,
                      dim_names = dim_names) |>
      broadcast(dim_names = names(dim_names))

    names(dim_names_new) <- dim_names[names[[i]]] |>
      tibble::as_tibble() |>
      tidyr::unpack(dplyr::all_of(names[[i]]), ...,
                    names_sep = names_sep) |>
      names()

    loc <- vec_as_location(names[[i]], length(dim_names), names(dim_names))
    dim_names <- dim_names[-loc]
    dim_names <- append(dim_names, dim_names_new,
                        after = loc - 1)
    dim <- unname(list_sizes(dim_names))

    if (is_ddf_col(.data)) {
      dim(data) <- dim
    } else {
      for (i in length(data)) {
        data_i <- as.array(data[[i]])
        dim(data_i) <- dim
        data[[i]] <- data_i
      }
    }
    attr(data, "dim_names") <- dim_names
  }
  data
}
