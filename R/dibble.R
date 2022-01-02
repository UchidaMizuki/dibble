new_dibble <- function(x, dim_names) {
  class(x) <- "dibble"
  attr(x, "dim_names") <- dim_names
  x
}

#' Build a dimensional data frame
#'
#' \code{dibble()} constructs the dimensional data frame called a dibble.
#'
#' @param ... A set of name-metric pairs, tibbles, or dibbles.
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
  dim_names <- expand_dim_names(dim_names)
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

#'
#'
#' @export
dibble_by <- function(x, ...) {
  nms <- names(tidyselect::eval_select(expr(c(...)), x))

  dim_names <- rep_along(nms, list(NULL))
  names(dim_names) <- nms

  as_dibble(x, dim_names)
}

#' Coerce a data frame to a dibble
#'
#' \code{as_dibble()} turns a data frame into a dimensional data frame called a dibble.
#'
#' @param x A data frame or a dibble.
#' @param ... Unused, for extensibility.
#'
#' @return A dibble.
#'
#' @export
as_dibble <- function(x, ...) {
  UseMethod("as_dibble")
}

#' @rdname as_dibble
#'
#' @param dim_names A list of dimension names.
#'
#' @export
as_dibble.data.frame <- function(x, dim_names, ...) {
  dim_names <- as_dim_names(dim_names, x)

  nms <- names(x)
  axes <- names(dim_names)
  old_axes <- intersect(nms, axes)
  meas_names <- setdiff(nms, axes)

  stopifnot(
    !vec_duplicate_any(x[old_axes])
  )

  id <- expand.grid(dim_names,
                    KEEP.OUT.ATTRS = FALSE,
                    stringsAsFactors = FALSE)
  x <- vec_slice(x[meas_names], vec_match(id[old_axes], x[old_axes]))

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

#' @export
nrow <- function(x) {
  UseMethod("nrow")
}
registerS3method("nrow", "default", base::nrow)

#' @export
nrow.dibble <- function(x) {
  nrow_dibble(x)
}

nrow_dibble <- function(x) {
  prod(dim(x))
}

#' @export
ncol <- function(x) {
  UseMethod("ncol")
}
registerS3method("ncol", "default", base::ncol)

#' @export
ncol.dibble <- function(x) {
  length(colnames(x))
}

#' @export
rownames <- function(x, ...) {
  UseMethod("rownames")
}
registerS3method("rownames", "default", base::rownames)

#' @export
rownames.dibble <- function(x, ...) {
  NULL
}

#' @export
colnames <- function(x, ...) {
  UseMethod("colnames")
}
registerS3method("colnames", "default", base::colnames)

#' @export
colnames.dibble <- function(x, ...) {
  names(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dibble <- function(x, ...) {
  as_tibble_dibble(x, ...)
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
  names(value) <- names(undibble(x)[i])
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
  names(value) <- names(undibble(x)[i])
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

  meas_names <- colnames(x)
  size_meas <- big_mark(length(meas_names))

  groups <- names(attr(x, "group_names"))

  x_head <- ungroup(head_dibble(x, n))

  df <- as_tibble_dibble(x_head)
  df <- new_data_frame(df,
                       class = c("dibble_impl", "tbl"))

  dim_sum <- c(Dimensions = commas(paste0(axes, " [", big_mark(dim), "]")))

  if (is_dibble(x) || is_grouped_dibble(x)) {
    tbl_sum <- c(`A dibble` = paste(big_mark(size_dim), size_meas,
                                    sep = " x "),
                 dim_sum,
                 Metrics = commas(meas_names))

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
