#' Build a dimensional data frame
#'
#' `dibble()` constructs a dimensional data frame called a dibble.
#'
#' Manipulation functions:
#'
#' * mutate()
#' * rename()
#' * select() & relocate()
#' * slice()
#'
#' @param ... A set of name-measure pairs.
#' @param .dim_names A list of dimension names.
#'
#' @return A dibble.
#'
#' @export
dibble <- function(...,
                   .dim_names = NULL) {
  args <- list2(...)

  old_dim_names <- union_dim_names(purrr::map(unname(args), dimnames))
  new_dim_names <- as_dim_names(.dim_names, old_dim_names)

  fun <- function(x) {
    if (is_dim_names(old_dim_names)) {
      if (is.null(.dim_names)) {
        x <- broadcast(x, old_dim_names)
      } else {
        x <- suppress_warning_broadcast(broadcast(x, old_dim_names))
      }
    }
    undibble(broadcast(x, new_dim_names))
  }

  args <- purrr::map2(unname(args), names2(args),
                      function(x, nm) {
                        if (is_tbl_ddf(x)) {
                          x <- purrr::modify(as.list(x), fun)

                          if (nm != "") {
                            stopifnot(
                              is_scalar_list(x)
                            )

                            names(x) <- nm
                          }
                          x
                        } else {
                          x <- list(fun(x))
                          names(x) <- nm
                          x
                        }
                      })
  args <- list_unchop(args)

  if (!is_named(args)) {
    stopifnot(
      is_scalar_list(args)
    )

    new_ddf_col(args[[1L]], new_dim_names)
  } else {
    new_tbl_ddf(args, new_dim_names)
  }
}

#' Constructs a dibble by one or more variables
#'
#' `dibble_by()` constructs a dibble by one or more variables.
#'
#' @param x A data frame or a dibble.
#' @param ... Variables.
#' @param .names_sep Passed to `tidyr::pack()`.
#'
#' @return A dibble.
#'
#' @export
dibble_by <- function(x, ...,
                      .names_sep = NULL) {
  args <- enquos(...)

  # pack data
  nms <- names2(args)
  loc <- nms != ""
  x <- tidyr::pack(x, !!!args[loc],
                   .names_sep = .names_sep)
  args[loc] <- as_quosures(nms[loc])
  args <- unname(args)

  as_dibble(dplyr::rowwise(x, !!!args))
}

#' Coerce an object to a dibble
#'
#' `as_dibble()` turns an object into a dimensional data frame called a dibble.
#'
#' @param x An object.
#' @param ... Unused, for extensibility.
#'
#' @return A dibble.
#'
#' @export
as_dibble <- function(x, ...) {
  UseMethod("as_dibble")
}

#' @rdname as_dibble
#' @export
as_dibble.default <- function(x, ...) {
  dim_names <- dimnames(x)
  stopifnot(
    is_dim_names(dim_names)
  )

  new_ddf_col(unname(x), dim_names)
}

#' @rdname as_dibble
#' @export
as_dibble.rowwise_df <- function(x, ...) {
  axes <- dplyr::group_vars(x)
  x <- dplyr::ungroup(x)
  haystack <- x[axes]
  stopifnot(
    !vec_duplicate_any(haystack)
  )

  dim_names <- purrr::map(haystack, unique)
  dim <- list_sizes_unnamed(dim_names)

  needles <- expand_grid_col_major(dim_names)
  x <- vec_slice(x[!names(x) %in% axes],
                 vec_match(needles, haystack))
  x <- purrr::map(x,
                  function(x) {
                    array(x, dim)
                  })
  new_tbl_ddf(x, dim_names)
}

#' @rdname as_dibble
#' @export
as_dibble.grouped_df <- function(x, ...) {
  as_dibble.rowwise_df(x, ...)
}

#' @rdname as_dibble
#' @export
as_dibble.ddf_col <- function(x, ...) {
  x
}

#' @rdname as_dibble
#' @export
as_dibble.tbl_ddf <- function(x, ...) {
  x
}

#' Test if the object is a dibble
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble <- function(x) {
  is_ddf_col(x) || is_tbl_ddf(x)
}

undibble <- function(x) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  x
}

dimnames_dibble <- function(x) {
  attr(x, "dim_names")
}

`dimnames<-_dibble` <- function(x, value) {
  dim_names <- dimnames(x)
  stopifnot(
    list_sizes(value) == list_sizes(dim_names)
  )

  attr(x, "dim_names") <- value
  x
}

dim_dibble <- function(x) {
  list_sizes_unnamed(dimnames(x))
}

as_tibble_dibble <- function(x, n) {
  dim_names <- tidyr::expand_grid(!!!dimnames(x))

  fun <- function(x) {
    as.vector(aperm(as.array(x)))
  }

  if (is_ddf_col(x)) {
    out <- vec_cbind(dim_names,
                     !!n := fun(undibble(x)),
                     .name_repair = "check_unique")
  } else {
    out <- purrr::modify(undibble(x), fun)

    if (!is.null(n)) {
      stopifnot(
        vec_size(out) == vec_size(n)
      )

      names(n)[names2(n) == ""] <- names(out)[!names(out) %in% names2(n)]
      names(out) <- n[names(out)]
    }

    out <- vec_cbind(dim_names, !!!out,
                     .name_repair = "check_unique")

  }
  out
}

aperm_dibble <- function(a, perm, ...) {
  dim_names <- dimnames(a)

  if (is.null(perm)) {
    new_dim_names <- rev(dim_names)
    perm <- NULL
  } else {
    new_dim_names <- dim_names[perm]

    if (is.character(perm)) {
      perm <- vec_match(names(new_dim_names), names(dim_names))
    }
  }

  class <- class(a)
  if (is_ddf_col(a)) {
    a <- aperm(as.array(a), perm, ...)
    new_ddf_col(a, new_dim_names,
                class = class)
  } else {
    a <- purrr::modify(undibble(a),
                       function(x) {
                         aperm(x, perm, ...)
                       })
    new_tbl_ddf(a, new_dim_names,
                class = class)
  }
}



# Verbs -------------------------------------------------------------------

slice_dibble <- function(.data, ...) {
  locs <- purrr::modify(list2(...),
                        function(x) {
                          x %||% missing_arg()
                        })
  nms <- names2(locs)

  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  stopifnot(
    vec_size(locs) == vec_size(dim_names),
    nms == "" | nms %in% axes
  )

  names(locs)[nms == ""] <- axes[!axes %in% nms]
  locs <- locs[axes]

  dim_names <- purrr::map2(dim_names, locs,
                           function(x, i) {
                             if (is_missing(i)) {
                               x
                             } else {
                               vec_slice(x, i)
                             }
                           })
  names(dim_names) <- axes

  class <- class(.data)
  if (is_ddf_col(.data)) {
    new_ddf_col(exec(`[`, .data, !!!locs,
                     drop = FALSE),
                dim_names = dim_names,
                class = class)
  } else if (is_tbl_ddf(.data)) {
    new_tbl_ddf(purrr::modify(undibble(.data),
                              function(x) {
                                exec(`[`, x, !!!locs,
                                     drop = FALSE)
                              }),
                dim_names = dim_names,
                class = class)
  }
}

select_dibble <- function(.data, ..., .relocate = FALSE) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  if (is_ddf_col(.data)) {
    data <- dim_names
  } else {
    data <- c(dim_names, .data)
  }
  nms <- names(tidyselect::eval_select(expr(c(...)), data))

  perm_match <- function(x, y) {
    vec_match(c(intersect(x, y), setdiff(y, x)), y)
  }

  if (is_tbl_ddf(.data)) {
    if (.relocate) {
      meas_names <- colnames(.data)
      .data <- .data[perm_match(nms, meas_names)]
    } else {
      .data <- .data[setdiff(nms, axes)]
    }
  }

  perm <- perm_match(nms, axes)
  aperm(.data, perm)
}

rename_dibble <- function(.data, ...) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  if (is_ddf_col(.data)) {
    data <- dim_names
  } else {
    data <- c(dim_names, .data)
  }
  nms <- names(data)
  names(nms) <- nms
  loc <- tidyselect::eval_rename(expr(c(...)), data)
  nms[loc] <- names(loc)

  names(dimnames(.data)) <- nms[axes]

  if (is_tbl_ddf(.data)) {
    meas_names <- colnames(.data)
    names(.data) <- nms[meas_names]
  }
  .data
}

#' @importFrom dplyr filter
#' @export
dplyr::filter

filter_dibble <- function(.data, ...) {
  args <- enquos(...)
  dim_names <- dimnames(.data)
  axes <- names(dim_names)
  idxs <- purrr::map_int(unname(args),
                         function(x) {
                           find_index_check(x, axes)
                         })

  size <- vec_size(dim_names)
  locs <- vec_init(list(), vec_size(dim_names))

  for (i in seq_along(args)) {
    idx <- idxs[[i]]
    loc <- locs[[idx]]

    new_loc <- eval_tidy(args[[i]], dim_names)
    new_loc <- new_loc & !is.na(new_loc)

    if (is.null(loc)) {
      locs[[idx]] <- new_loc
    } else {
      locs[[idx]] <- loc & new_loc
    }
  }
  slice(.data, !!!locs)
}

find_index_check <- function(x, names) {
  out <- find_index(quo_get_expr(x), names)
  stopifnot(
    vec_size(out) == 1L
  )
  out
}

find_index <- function(x, names) {
  if (is_atomic(x)) {
    integer()
  } else if (is_symbol(x) || x[[1L]] == "$") {
    which(head_symbol(x) == names)
  } else {
    stopifnot(is_call(x))

    out <- purrr::map(as.list(x[-1L]),
                      \(x) find_index(x,
                                      names = names))
    list_unchop(out)
  }
}

head_symbol <- function(x) {
  while (!is_symbol(x)) {
    lhs <- f_lhs(x)

    if (lhs == ".data") {
      x <- f_rhs(x)
    } else {
      x <- lhs
    }
  }
  x
}

# Printing ----------------------------------------------------------------

print_dibble <- function(x, n, ...) {
  writeLines(format(x, n = n, ...))
  invisible(x)
}

format_dibble <- function(x, n, ...) {
  n <- get_n_print(n, nrow(x))

  setup <- tbl_format_setup(x,
                            n = n,
                            ...)
  header <- tbl_format_header(x, setup)
  body <- tbl_format_body(x, setup)
  footer <- tbl_format_footer(x, setup)
  c(header, body, footer)
}

tbl_format_setup_dibble <- function(x, n, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- list_sizes_unnamed(dim_names)
  size_dim <- prod(dim)

  meas_names <- colnames(x)
  size_meas <- big_mark(vec_size(meas_names))

  dim_sum <- c(`Dimensions` = commas(paste0(axes, " [", big_mark(dim), "]")))
  if (is_ddf_col(x)) {
    tbl_sum <- c(`A dibble` = big_mark(size_dim),
                             dim_sum)
  } else {
    tbl_sum <- c(`A dibble` = paste(big_mark(size_dim), size_meas,
                                    sep = " x "),
                 dim_sum,
                 `Measures` = commas(meas_names))
  }

  x <- tibble::as_tibble(head_dibble(x,
                                     n = n))
  setup <- tbl_format_setup(x,
                            n = n,
                            ...)
  setup$tbl_sum <- tbl_sum
  rows_total_old <- setup$rows_total
  rows_total_new <- size_dim
  setup$rows_total <- rows_total_new
  setup$rows_missing <- setup$rows_missing + rows_total_new - rows_total_old
  setup
}

tbl_format_header_dibble <- function(x, setup, ...) {
  x <- setup$x
  tbl_format_header(x, setup, ...)
}

tbl_format_body_dibble <- function(x, setup, ...) {
  x <- setup$x
  tbl_format_body(x, setup, ...)
}

tbl_format_footer_dibble <- function(x, setup, ...) {
  x <- setup$x
  tbl_format_footer(x, setup, ...)
}

head_dibble <- function(x, n) {
  dim <- rev(dim(x))

  loc <- rep(1, vec_size(dim))
  i <- cumprod(dim) < n
  dim <- dim[i]

  loc[i] <- dim

  if (!all(i)) {
    loc[[which(!i)[[1L]]]] <- ceiling(n / prod(dim))
  }
  loc <- rev(purrr::map(loc, seq_len))

  slice(x, !!!loc)
}
