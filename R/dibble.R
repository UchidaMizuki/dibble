new_dibble <- function(x, dim_names) {
  class(x) <- "dibble"
  attr(x, "dim_names") <- dim_names
  x
}

#' Build a dimensional data frame
#'
#' `dibble()` constructs a dimensional data frame called a dibble.
#'
#' @param ... A set of name-metric pairs.
#' @param .dim_names A list of dimension names.
#'
#' @return A dibble.
#'
#' @export
dibble <- function(...,
                   .dim_names = NULL) {
  dots <- enquos(...)
  dots <- purrr::modify(as.list(dots),
                        function(x) {
                          x <- supress_warning_broadcast(eval_tidy(x))

                          if (is.data.frame(x)) {
                            x <- as_dibble(x, .dim_names)
                          }
                          x
                        })

  dim_names <- purrr::map(dots, dimnames)
  dim_names <- union_dim_names(dim_names)
  dim_names <- as_dim_names(.dim_names, dim_names)

  dots <- purrr::modify(dots,
                        function(x) {
                          if (is_dibble(x) || is_grouped_dibble(x)) {
                            x <- ungroup(x)
                            as.list(x)
                          } else {
                            list(x)
                          }
                        })
  dots <- exec(c, !!!dots)

  stopifnot(
    is_named(dots)
  )

  dots <- purrr::modify(dots,
                        function(x) {
                          undibble(dibble_metric(x, dim_names))
                        })
  new_dibble(dots, dim_names)
}

#' Constructs a dibble by one or more variables
#'
#' `dibble_by()` constructs a dibble by one or more variables.
#'
#' @param x A data frame or a dibble.
#' @param ... Variables.
#'
#' @export
dibble_by <- function(x, ...) {
  nms <- names(tidyselect::eval_select(expr(c(...)), x))

  dim_names <- rep_along(nms, list(NULL))
  names(dim_names) <- nms

  as_dibble(x, dim_names)
}

#' Coerce an object to a dibble
#'
#' `as_dibble()` turns an object into a dimensional data frame called a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
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
as_dibble.data.frame <- function(x, dim_names, ...) {
  x <- as.data.frame(x)
  dim_names <- as_dim_names(dim_names, x)

  nms <- names(x)
  axes <- names(dim_names)
  old_axes <- intersect(nms, axes)
  met_names <- setdiff(nms, axes)

  stopifnot(
    !vec_duplicate_any(x[old_axes])
  )

  id <- expand.grid(dim_names,
                    KEEP.OUT.ATTRS = FALSE,
                    stringsAsFactors = FALSE)
  x <- vec_slice(x[met_names], vec_match(id[old_axes], x[old_axes]))

  dim <- lengths(dim_names)
  x <- purrr::modify(as.list(x),
                     function(x) {
                       array(x, dim)
                     })
  new_dibble(x, dim_names)
}

#' @rdname as_dibble
#' @export
as_dibble.dibble <- function(x, dim_names, ...) {
  dibble(x, .dim_names = dim_names)
}

#' Test if the object is a dibble
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble <- function(x) {
  inherits(x, "dibble")
}

#' @export
as.list.dibble <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- undibble(x)
  purrr::modify(x,
                function(x) {
                  new_dibble_metric(x, dim_names)
                })
}

undibble <- function(x) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  attr(x, "group_names") <- NULL
  x
}

#' @export
dimnames.dibble <- function(x) {
  dimnames_dibble(x)
}

dimnames_dibble <- function(x) {
  attr(x, "dim_names")
}

#' @export
`dimnames<-.dibble` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

`dimnames<-_dibble` <- function(x, value) {
  if (is_dibble(x) || is_dibble_metric(x)) {
    attr(x, "dim_names") <- as_dim_names(value, dimnames(x))
    x
  } else if (is_grouped_dibble(x)) {
    group_names <- group_names(x)
    loc <- seq_along(group_names)

    group_names <- as_dim_names(value[loc], group_names)

    x <- purrr::modify(undibble(x),
                       function(x) {
                         purrr::modify(x,
                                       function(x) {
                                         dimnames(x) <- value[-loc]
                                         x
                                       })
                       })
    new_grouped_dibble(x, group_names)
  }
}

#' @export
dim.dibble <- function(x) {
  lengths(dimnames(x))
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
nrow.dibble <- function(x, ...) {
  nrow_dibble(x)
}

nrow_dibble <- function(x) {
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
ncol.dibble <- function(x, ...) {
  length(colnames(x))
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
rownames.dibble <- function(x, ...) {
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
colnames.dibble <- function(x, ...) {
  names(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dibble <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

as_tibble_dibble <- function(x, ...) {
  dim <- exec(tidyr::expand_grid, !!!dimnames(x))

  if (is_grouped_dibble(x)) {
    x <- ungroup(x)
  }

  if (is_dibble(x)) {
    met <- purrr::modify(undibble(x), as_met)
    vctrs::vec_cbind(dim, !!!met, ...,
                     .name_repair = "check_unique")
  } else if (is_dibble_metric(x)) {
    vctrs::vec_cbind(dim,
                     . = as_met(x), ...,
                     .name_repair = "check_unique")
  }
}

as_met <- function(x) {
  as.vector(aperm(as.array(x)))
}

#' @export
aperm.dibble <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}

aperm_dibble <- function(a, perm, ...) {
  if (is.null(perm)) {
    dim_names <- rev(dimnames(a))
  } else {
    dim_names <- dimnames(a)[perm]
  }

  if (is_dibble(a)) {
    a <- purrr::modify(undibble(a),
                       function(x) {
                         aperm(x, perm, ...)
                       })
    new_dibble(a, dim_names)
  } else if (is_dibble_metric(a)) {
    a <- aperm(undibble(a), perm, ...)
    new_dibble_metric(a, dim_names)
  }
}



# Subsetting --------------------------------------------------------------

#' @export
`[.dibble` <- function(x, i) {
  new_dibble(NextMethod(), dimnames(x))
}

#' @export
`[<-.dibble` <- function(x, i, value) {
  `[<-_dibble`(x, i, value)
}

`[<-_dibble` <- function(x, i, value) {
  names(value) <- dplyr::coalesce(names(undibble(x)[i]), i)
  mutate(x, !!!value)
}

#' @export
`[[.dibble` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}

#' @export
`[[<-.dibble` <- function(x, i, value) {
  `[[<-_dibble`(x, i, value)
}

`[[<-_dibble` <- function(x, i, value) {
  value <- list(value)
  names(value) <- dplyr::coalesce(names(undibble(x)[i]), i)
  mutate(x, !!!value)
}

#' @export
`$.dibble` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}

#' @export
`$<-.dibble` <- function(x, i, value) {
  `$<-_dibble`(x, i, value)
}

`$<-_dibble` <- function(x, i, value) {
  x[[i]] <- value
  x
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.dibble <- function(.data, ...) {
  slice_dibble(.data, ...)
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

#' @importFrom dplyr mutate
#' @export
mutate.dibble <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)

  dim_names <- dimnames(.data)
  .data <- undibble(.data)

  data <- as.list(.data)

  for (i in seq_along(nms)) {
    nm <- nms[[i]]

    data_nm <- eval_tidy(dots[[i]], data)
    data_nm <- dibble_metric(data_nm, dim_names)

    data[[nm]] <- data_nm
    .data[[nm]] <- undibble(data_nm)
  }
  new_dibble(.data, dim_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dibble <- function(x, ...) {
  x
}

#' @importFrom dplyr select
#' @export
select.dibble <- function(.data, ...) {
  select_dibble(.data, ...)
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
      met_names <- colnames(.data)
      .data <- .data[perm_match(nms, met_names)]
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

#' @importFrom dplyr relocate
#' @export
relocate.dibble <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.dibble <- function(.data, ...) {
  rename_dibble(.data, ...)
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
    met_names <- colnames(.data)
    names(.data) <- nms[met_names]
  }
  .data
}



# Printing ----------------------------------------------------------------

#' @export
print.dibble <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}

print_dibble <- function(x, n) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- lengths(dim_names)
  size_dim <- prod(dim)

  met_names <- colnames(x)
  size_met <- big_mark(length(met_names))

  groups <- names(attr(x, "group_names"))

  x_head <- ungroup(head_dibble(x, n))

  df <- as_tibble_dibble(x_head)
  df <- new_data_frame(df,
                       class = c("dibble_impl", "tbl"))

  dim_sum <- c(Dimensions = commas(paste0(axes, " [", big_mark(dim), "]")))

  if (is_dibble(x) || is_grouped_dibble(x)) {
    tbl_sum <- c(`A dibble` = paste(big_mark(size_dim), size_met,
                                    sep = " x "),
                 dim_sum,
                 Metrics = commas(met_names))

    if (!is.null(groups)) {
      size_groups <- big_mark(prod(dim[groups]))
      tbl_sum <- c(tbl_sum,
                   Groups = paste0(commas(groups), " [", size_groups, "]"))
    }

    attr(df, "tbl_sum") <- tbl_sum
  } else if (is_dibble_metric(x)) {
    attr(df, "tbl_sum") <- c(`A metric` = big_mark(size_dim),
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
    loc[[which(!i)[[1]]]] <- ceiling(n / prod(dim))
  }
  loc <- rev(purrr::map(loc, seq_len))

  slice(x, !!!loc)
}

#' @importFrom pillar tbl_format_setup
#' @export
tbl_format_setup.dibble_impl <- function(x, width, ..., n, max_extra_cols, max_footer_lines) {
  setup <- NextMethod()

  setup$tbl_sum <-  attr(x, "tbl_sum")

  rows_total_old <- setup$rows_total
  rows_total_new <- attr(x, "rows_total")
  setup$rows_total <- rows_total_new
  setup$rows_missing <- rows_total_new - (rows_total_old - setup$rows_missing)
  setup
}
