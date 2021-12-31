new_dibble <- function(x, dim_names) {
  class(x) <- "dibble"
  attr(x, "dim_names") <- dim_names
  x
}

dibble <- function(..., .dim_names = NULL) {
  # TODO
}

#' @export
dibble_by <- function(x, ...) {
  dots <- rlang::enquos(...)
  nms <- rlang::names2(dots)
  dots <- rlang::quos_auto_name(dots)

  size <- length(dots)
  dim_names <- vector("list", size)
  names(dim_names) <- names(dots)

  for (i in seq_len(size)) {
    if (nms[[i]] == "") {
      dim_names[i] <- list(NULL)
    } else {
      dim_names[[i]] <- rlang::eval_tidy(dots[[i]])
    }
  }

  as_dibble(x, dim_names)
}

#' @export
as_dibble <- function(x, ...) {
  UseMethod("as_dibble")
}

#' @export
as_dibble.data.frame <- function(x, dim_names, ...) {
  dim_names <- as_dim_names(dim_names, x)

  axes <- names(dim_names)
  meas_names <- setdiff(names(x), axes)

  stopifnot(
    !vec_duplicate_any(x[axes])
  )

  id <- expand.grid(dim_names,
                    KEEP.OUT.ATTRS = FALSE,
                    stringsAsFactors = FALSE)
  x <- vec_slice(x[meas_names], vec_match(id, x[axes]))

  dim <- lengths(dim_names)
  x <- purrr::modify(as.list(x),
                     function(x) {
                       array(x, dim)
                     })
  new_dibble(x, dim_names)
}

is_dibble <- function(x) {
  inherits(x, "dibble")
}

#' @export
as.list.dibble <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- as_list_dibble(x)
  purrr::modify(x,
                function(x) {
                  new_dibble_measure(x, dim_names)
                })
}

as_list_dibble <- function(x) {
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
  assign_dimnames_dibble(x, value)
}

assign_dimnames_dibble <- function(x, value) {
  attr(x, "dim_names") <- as_dim_names(value, dimnames(x))
  x
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
    a <- purrr::modify(as_list_dibble(a),
                       function(x) {
                         aperm(x, perm, ...)
                       })
    new_dibble(a, dim_names)
  } else if (is_dibble_measure(a)) {
    a <- aperm(as.array(a), perm, ...)
    new_dibble_measure(a, dim_names)
  }
}



# Subsetting --------------------------------------------------------------

#' @export
`[.dibble` <- function(x, i) {
  new_dibble(NextMethod(), dimnames(x))
}

#' @export
`[[.dibble` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}

#' @export
`$.dibble` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
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
  dots <- rlang::enquos(..., .named = TRUE)
  nms <- names(dots)

  dim_names <- dimnames(.data)
  data <- as.list(.data)

  for (i in seq_along(nms)) {
    nm <- nms[[i]]

    data_nm <- rlang::eval_tidy(dots[[i]], data)
    data_nm <- dibble_measure(data_nm, dim_names)

    data[[nm]] <- data_nm
    .data[[nm]] <- as.array(data_nm)
  }
  .data
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
                 Measures = commas(meas_names))

    if (!is.null(groups)) {
      size_groups <- big_mark(prod(dim[groups]))
      tbl_sum <- c(tbl_sum,
                   Groups = paste0(commas(groups), " [", size_groups, "]"))
    }

    attr(df, "tbl_sum") <- tbl_sum
  } else if (is_dibble_measure(x)) {
    attr(df, "tbl_sum") <- c(`A measure` = big_mark(size_dim),
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
