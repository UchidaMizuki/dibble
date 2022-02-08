new_tbl_ddf <- function(x, dim_names) {
  structure(x,
            dim_names = dim_names,
            class = "tbl_ddf")
}

is_tbl_ddf <- function(x) {
  inherits(x, "tbl_ddf")
}

#' @export
as.list.tbl_ddf <- function(x, ...) {
  dim_names <- dimnames(x)
  lapply(undibble(x),
         function(x) {
           new_ddf_col(x, dim_names)
         })
}

#' @export
dimnames.tbl_ddf <- function(x) {
  dimnames_dibble(x)
}

dimnames_dibble <- function(x) {
  attr(x, "dim_names")
}

#' @export
`dimnames<-.tbl_ddf` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

`dimnames<-_dibble` <- function(x, value) {
  if (is_grouped_ddf(x)) {
    group_dim_names <- group_dim_names(x)

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
    new_grouped_dibble(x, new_group_dim_names)
  } else {
    dim_names <- dimnames(x)
    stopifnot(
      list_sizes(value) == list_sizes(dim_names)
    )

    attr(x, "dim_names") <- value
    x
  }
}

#' @export
dim.tbl_ddf <- function(x) {
  list_sizes(dimnames(x))
}

#' The number of rows/columns
#'
#' `nrow()` and `ncol()` return the number of rows or columns present in x.
#'
#' @param x An object.
#' @param ... Other arguments passed on to methods.
#'
#' @return An integer or `NULL`.
#'
#' @name nrow-ncol
NULL

#' @rdname nrow-ncol
#' @export
nrow <- function(x, ...) {
  UseMethod("nrow")
}

#' @rdname nrow-ncol
#' @export
nrow.default <- function(x, ...) {
  base::nrow(x)
}

#' @rdname nrow-ncol
#' @export
nrow.tbl_ddf <- function(x, ...) {
  nrow_dibble(x, ...)
}

nrow_dibble <- function(x, ...) {
  prod(dim(x))
}

#' @rdname nrow-ncol
#' @export
ncol <- function(x, ...) {
  UseMethod("ncol")
}

#' @rdname nrow-ncol
#' @export
ncol.default <- function(x, ...) {
  base::ncol(x)
}

#' @rdname nrow-ncol
#' @export
ncol.tbl_ddf <- function(x, ...) {
  vec_size(colnames(x))
}

#' Row and column names
#'
#' Retrieve or set the row or column names of a matrix-like object.
#'
#' @param x A matrix-like object.
#' @param ... Other arguments passed on to methods.
#'
#' @return A list of row/column names.
#'
#' @name row-colnames
NULL

#' @rdname row-colnames
#' @export
rownames <- function(x, ...) {
  UseMethod("rownames")
}

#' @rdname row-colnames
#' @export
rownames.default <- function(x, ...) {
  base::rownames(x, ...)
}

#' @rdname row-colnames
#' @export
rownames.tbl_ddf <- function(x, ...) {
  NULL
}

#' @rdname row-colnames
#' @export
colnames <- function(x, ...) {
  UseMethod("colnames")
}

#' @rdname row-colnames
#' @export
colnames.default <- function(x, ...) {
  base::colnames(x, ...)
}

#' @rdname row-colnames
#' @export
colnames.tbl_ddf <- function(x, ...) {
  names(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.tbl_ddf <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

as_tibble_dibble <- function(x, ..., n) {
  dim_names <- rev(rlang::exec(expand.grid, !!!rev(dimnames(x)),
                               KEEP.OUT.ATTRS = FALSE,
                               stringsAsFactors = FALSE))

  fun <- function(x) {
    as.vector(aperm(as.array(x)))
  }

  if (is_dibble(x)) {
    x <- vec_cbind(dim_names, !!!lapply(undibble(x), fun),
                   .name_repair = "check_unique")
  } else if (is_dibble_measure(x)) {
    x <- vec_cbind(dim_names,
                   !!n := fun(undibble(x)),
                   .name_repair = "check_unique")
  }
  as_tibble(x, ...)
}

#' @export
aperm.tbl_ddf <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
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

  if (is_dibble(a)) {
    a <- lapply(undibble(a),
                function(x) {
                  aperm(x, perm, ...)
                })
    new_tbl_ddf(a, dim_names)
  } else if (is_dibble_measure(a)) {
    a <- aperm(as.array(a), perm, ...)
    new_ddf_col(a, dim_names)
  }
}



# Subsetting --------------------------------------------------------------

#' @export
`[.tbl_ddf` <- function(x, i) {
  new_dibble(NextMethod(), dimnames(x))
}

#' @export
`[[.tbl_ddf` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}

#' @export
`$.tbl_ddf` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.tbl_ddf <- function(.data, ...) {
  slice_dibble(.data, ...)
}

slice_dibble <- function(.data, ...) {
  loc <- lapply(rlang::list2(...),
                function(x) {
                  x %||% missing_arg()
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
  dim_names <- mapply(dim_names, loc,
                      FUN = `[`,
                      SIMPLIFY = FALSE)
  names(dim_names) <- axes

  if (is_ddf_col(.data)) {
    new_ddf_col(rlang::exec(`[`, .data, !!!loc,
                            drop = FALSE),
                dim_names = dim_names)
  } else if (is_tbl_ddf(.data)) {
    new_tbl_ddf(lapply(undibble(.data),
                       function(x) {
                         rlang::exec(`[`, x, !!!loc,
                                     drop = FALSE)
                       }),
                dim_names = dim_names)
  } else if (is_grouped_ddf(.data)) {
    group_dim_names <- group_dim_names(.data)
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
                      x <- rlang::exec(`[`, x, !!!loc_groups,
                                       drop = FALSE)
                      lapply(x,
                             function(x) {
                               slice(x, !!!loc)
                             })
                    })
    new_grouped_ddf(.data, group_dim_names)
  }
}

#' @importFrom dplyr mutate
#' @export
mutate.tbl_ddf <- function(.data, ...) {
  dots <- rlang::enquos(...,
                        .named = TRUE)
  nms <- names(dots)

  dim_names <- dimnames(.data)
  data <- as.list(.data)

  .data <- undibble(.data)

  for (i in vec_seq_along(nms)) {
    nm <- nms[[i]]

    data_nm <- ddf_col(rlang::eval_tidy(dots[[i]], data),
                       dim_names = dim_names)

    data[[nm]] <- data_nm
    .data[[nm]] <- undibble(data_nm)
  }
  new_tbl_ddf(.data, dim_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_ddf <- function(x, ...) {
  x
}

#' @importFrom dplyr select
#' @export
select.tbl_ddf <- function(.data, ...) {
  select_dibble(.data, ...)
}

select_dibble <- function(.data, ..., .relocate = FALSE) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  group_dim_names <- group_dim_names(.data)
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

#' @importFrom dplyr relocate
#' @export
relocate.tbl_ddf <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.tbl_ddf <- function(.data, ...) {
  rename_dibble(.data, ...)
}

rename_dibble <- function(.data, ...) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  group_dim_names <- group_dim_names(.data)
  group_axes <- names(group_dim_names)

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

#' @export
print.tbl_ddf <- function(x, n = NULL, ...) {
  print_dibble(x, n, ...)
}

print_dibble <- function(x, n, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- list_sizes(dim_names)
  size_dim <- prod(dim)

  meas_names <- colnames(x)
  size_meas <- big_mark(length(meas_names))

  group_dim_names <- group_dim_names(x)

  x_head <- ungroup(head_dibble(x, n))

  df <- new_data_frame(as_tibble(x_head),
                       class = c("tbl_dibble", "tbl"))

  dim_sum <- c(`Dimensions` = commas(paste0(axes, " [", big_mark(dim), "]")))

  if (is_dibble(x) || is_grouped_dibble(x)) {
    tbl_sum <- c(`A dibble` = paste(big_mark(size_dim), size_meas,
                                    sep = " x "),
                 dim_sum,
                 `Measures` = commas(meas_names))

    if (!is.null(group_dim_names)) {
      size_groups <- big_mark(prod(list_sizes(group_dim_names)))
      tbl_sum <- c(tbl_sum,
                   Groups = paste0(commas(names(group_dim_names)), " [", size_groups, "]"))
    }

    attr(df, "tbl_sum") <- tbl_sum
  } else if (is_dibble_measure(x)) {
    attr(df, "tbl_sum") <- c(`A dibble` = big_mark(size_dim),
                             dim_sum)
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
