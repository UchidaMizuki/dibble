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
  dots <- list2(...)

  old_dim_names <- union_dim_names(lapply(unname(dots), dimnames))
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

  dots <- mapply(dots, names2(dots),
                 FUN = function(x, nm) {
                   if (is_tbl_ddf(x)) {
                     x <- lapply(as.list(x), fun)

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
                 },
                 SIMPLIFY = FALSE,
                 USE.NAMES = FALSE)
  dots <- vec_c(!!!dots)

  if (!is_named(dots)) {
    stopifnot(
      is_scalar_list(dots)
    )

    new_ddf_col(dots[[1L]], new_dim_names)
  } else {
    new_tbl_ddf(dots, new_dim_names)
  }
}

#' Constructs a dibble by one or more variables
#'
#' `dibble_by()` constructs a dibble by one or more variables.
#'
#' @param x A data frame or a dibble.
#' @param ... Variables.
#'
#' @return A dibble.
#'
#' @export
dibble_by <- function(x, ...) {
  args <- enquos(...)

  # pack data
  nms <- names2(args)
  loc <- nms != ""
  x <- tidyr::pack(x, !!!args[loc])
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

  dim_names <- lapply(haystack, vec_unique)
  dim <- list_sizes_unnamed(dim_names)

  needles <- expand_grid_col_major(!!!dim_names)
  x <- vec_slice(x[!names(x) %in% axes],
                 vec_match(needles, haystack))
  x <- lapply(x,
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
    out <- lapply(undibble(x), fun)

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

  if (is_ddf_col(a)) {
    a <- aperm(as.array(a), perm, ...)
    new_ddf_col(a, dim_names)
  } else {
    a <- lapply(undibble(a),
                function(x) {
                  aperm(x, perm, ...)
                })
    new_tbl_ddf(a, dim_names)
  }
}



# Verbs -------------------------------------------------------------------

slice_dibble <- function(.data, ...) {
  locs <- lapply(list2(...),
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

  dim_names <- mapply(dim_names, locs,
                      FUN = function(x, i) {
                        if (is_missing(i)) {
                          x
                        } else {
                          vec_slice(x, i)
                        }
                      },
                      SIMPLIFY = FALSE)
  names(dim_names) <- axes

  if (is_ddf_col(.data)) {
    new_ddf_col(exec(`[`, .data, !!!locs,
                     drop = FALSE),
                dim_names = dim_names)
  } else if (is_tbl_ddf(.data)) {
    new_tbl_ddf(lapply(undibble(.data),
                       function(x) {
                         exec(`[`, x, !!!locs,
                              drop = FALSE)
                       }),
                dim_names = dim_names)
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
  idxs <- vapply(args,
                 function(x) {
                   find_index_check(x, axes)
                 },
                 FUN.VALUE = integer(1),
                 USE.NAMES = FALSE)

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
    vec_size(out) == 1
  )
  out
}

find_index <- function(x, names) {
  if (is_atomic(x)) {
    integer()
  } else if (is_symbol(x) || x[[1]] == "$") {
    which(head_symbol(x) == names)
  } else {
    stopifnot(is_call(x))

    out <- lapply(x[-1], find_index,
                  names = names)
    vec_c(!!!out)
  }
}

head_symbol <- function(x) {
  while (!is_symbol(x)) {
    x <- x[[2L]]
  }
  x
}

# Printing ----------------------------------------------------------------

print_dibble <- function(x, n, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- list_sizes_unnamed(dim_names)
  size_dim <- prod(dim)

  meas_names <- colnames(x)
  size_meas <- big_mark(vec_size(meas_names))

  df <- new_data_frame(as_tibble(head_dibble(x, n)),
                       class = c("tbl_dibble", "tbl"))

  dim_sum <- c(`Dimensions` = commas(paste0(axes, " [", big_mark(dim), "]")))

  if (is_ddf_col(x)) {
    attr(df, "tbl_sum") <- c(`A dibble` = big_mark(size_dim),
                             dim_sum)
  } else {
    tbl_sum <- c(`A dibble` = paste(big_mark(size_dim), size_meas,
                                    sep = " x "),
                 dim_sum,
                 `Measures` = commas(meas_names))

    attr(df, "tbl_sum") <- tbl_sum
  }

  attr(df, "rows_total") <- size_dim
  print(df)

  invisible(x)
}

head_dibble <- function(x, n) {
  # pillar:::get_pillar_option_print_max() + 1
  n <- n %||% 21
  dim <- rev(dim(x))

  loc <- rep(1, vec_size(dim))
  i <- cumprod(dim) < n
  dim <- dim[i]

  loc[i] <- dim

  if (!all(i)) {
    loc[[which(!i)[[1L]]]] <- ceiling(n / prod(dim))
  }
  loc <- rev(lapply(loc, seq_len))

  slice(x, !!!loc)
}

#' @importFrom pillar tbl_format_setup
#' @export
tbl_format_setup.tbl_dibble <- function(x, width, ..., n, max_extra_cols, max_footer_lines) {
  setup <- NextMethod()

  setup$tbl_sum <-  attr(x, "tbl_sum")

  rows_total_old <- setup$rows_total
  rows_total_new <- attr(x, "rows_total")
  setup$rows_total <- rows_total_new
  setup$rows_missing <- rows_total_new - (rows_total_old - setup$rows_missing)
  setup
}
