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

  old_dim_names <- union_dim_names(!!!lapply(unname(dots), dimnames))
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
                   if (is_tbl_ddf(x) || is_grouped_ddf(x)) {
                     x <- lapply(as.list(ungroup(x)), fun)

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
  as_dibble(dplyr::rowwise(x, ...))
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
  x <- ungroup(x)
  haystack <- x[axes]
  stopifnot(
    !vec_duplicate_any(haystack)
  )

  dim_names <- lapply(haystack, vec_unique)
  dim <- list_sizes(dim_names)

  needles <- expand.grid(dim_names,
                         KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = FALSE)
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

#' @rdname as_dibble
#' @export
as_dibble.grouped_ddf <- function(x, ...) {
  ungroup(x)
}

#' Test if the object is a dibble
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble <- function(x) {
  is_ddf_col(x) || is_tbl_ddf(x) || is_grouped_ddf(x)
}

are_dibble <- function(x) {
  vapply(x, is_dibble,
         logical(1))
}

undibble <- function(x) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  attr(x, "group_dim_names") <- NULL
  x
}

dimnames_dibble <- function(x) {
  attr(x, "dim_names")
}

`dimnames<-_dibble` <- function(x, value) {
  if (is_grouped_ddf(x)) {
    group_dim_names <- group_keys(x)

    loc <- seq_along(group_dim_names)
    new_group_dim_names <- value[loc]
    new_dim_names <- value[-loc]
    stopifnot(
      list_sizes(new_group_dim_names) == list_sizes(group_dim_names)
    )

    x <- lapply(undibble(x),
                function(x) {
                  lapply(x,
                         function(x) {
                           dimnames(x) <- new_dim_names
                           x
                         })
                })
    new_grouped_ddf(x, new_group_dim_names)
  } else {
    dim_names <- dimnames(x)
    stopifnot(
      list_sizes(value) == list_sizes(dim_names)
    )

    attr(x, "dim_names") <- value
    x
  }
}

dim_dibble <- function(x) {
  list_sizes(dimnames(x))
}

as_tibble_dibble <- function(x, ..., n) {
  dim_names <- rev(exec(expand.grid, !!!rev(dimnames(x)),
                        KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE))

  fun <- function(x) {
    as.vector(aperm(as.array(x)))
  }

  if (is_ddf_col(x)) {
    out <- vec_cbind(dim_names,
                     !!n := fun(undibble(x)),
                     .name_repair = "check_unique")
  } else {
    out <- vec_cbind(dim_names, !!!lapply(undibble(x), fun),
                     .name_repair = "check_unique")

  }

  out <- as_tibble(out, ...)

  if (is_grouped_ddf(x)) {
    out <- group_by(out, dplyr::all_of(group_vars(x)))
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
  loc <- lapply(list2(...),
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
  dim_names <- mapply(dim_names, loc,
                      FUN = `[`,
                      SIMPLIFY = FALSE)
  names(dim_names) <- axes

  if (is_ddf_col(.data)) {
    new_ddf_col(exec(`[`, .data, !!!loc,
                     drop = FALSE),
                dim_names = dim_names)
  } else if (is_tbl_ddf(.data)) {
    new_tbl_ddf(lapply(undibble(.data),
                       function(x) {
                         exec(`[`, x, !!!loc,
                              drop = FALSE)
                       }),
                dim_names = dim_names)
  } else if (is_grouped_ddf(.data)) {
    group_dim_names <- group_keys(.data)
    group_axes <- names(group_dim_names)
    groups <- seq_along(group_dim_names)

    loc_groups <- loc[groups]
    loc <- loc[-groups]

    group_dim_names <- mapply(group_dim_names, loc_groups,
                              FUN = function(x, i) {
                                x[i]
                              },
                              SIMPLIFY = FALSE)
    names(group_dim_names) <- group_axes

    .data <- lapply(undibble(.data),
                    function(x) {
                      x <- exec(`[`, x, !!!loc_groups,
                                drop = FALSE)
                      lapply(x,
                             function(x) {
                               slice(x, !!!loc)
                             })
                    })
    new_grouped_ddf(.data, group_dim_names)
  }
}

select_dibble <- function(.data, ..., .relocate = FALSE) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  group_dim_names <- group_keys(.data)
  group_axes <- names(group_dim_names)

  if (is_ddf_col(.data)) {
    data <- dim_names
  } else {
    data <- c(dim_names, .data)
  }
  nms <- names(tidyselect::eval_select(expr(c(...)), data))

  perm_match <- function(x, y) {
    vec_match(c(intersect(x, y), setdiff(y, x)), y)
  }

  if (is_tbl_ddf(.data) || is_grouped_ddf(.data)) {
    if (.relocate) {
      meas_names <- colnames(.data)
      .data <- .data[perm_match(nms, meas_names)]
    } else {
      .data <- .data[setdiff(nms, axes)]
    }
  }

  if (is_grouped_ddf(.data)) {
    perm_groups <- perm_match(nms, group_axes)
    axes <- setdiff(axes, group_axes)
    perm <- perm_match(nms, axes)

    group_dim_names <- group_dim_names[perm_groups]
    .data <- lapply(undibble(.data),
                    function(x) {
                      x <- aperm(x, perm_groups)
                      lapply(x,
                             function(x) {
                               aperm(x, perm)
                             })
                    })
    new_grouped_ddf(.data, group_dim_names)

  } else if (is_tbl_ddf(.data) || is_ddf_col(.data)) {
    perm <- perm_match(nms, axes)
    aperm(.data, perm)
  }
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

  if (is_tbl_ddf(.data) || is_grouped_ddf(.data)) {
    meas_names <- colnames(.data)
    names(.data) <- nms[meas_names]
  }
  .data
}

# Printing ----------------------------------------------------------------

print_dibble <- function(x, n, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- list_sizes(dim_names)
  size_dim <- prod(dim)

  meas_names <- colnames(x)
  size_meas <- big_mark(length(meas_names))

  x_head <- head_dibble(x, n)
  if (is_grouped_ddf(x_head)) {
    x_head <- ungroup(x_head)
  }

  df <- new_data_frame(as_tibble(x_head),
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

    if (is_grouped_ddf(x)) {
      group_dim_names <- group_keys(x)
      size_groups <- big_mark(prod(list_sizes(group_dim_names)))
      tbl_sum <- c(tbl_sum,
                   Groups = paste0(commas(names(group_dim_names)), " [", size_groups, "]"))
    }

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

  loc <- rep(1, length(dim))
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
