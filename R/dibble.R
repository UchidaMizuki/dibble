new_dibble <- function(x, dim_names) {
  class(x) <- "tbl_dim"
  attr(x, "dim_names") <- dim_names
  x
}

dibble <- function() {
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
as_dibble.data.frame <- function(x, dim_names, col_names = NULL, ...) {
  dim_names <- as_dim_names(dim_names, x)

  axes <- names(dim_names)
  col_names <- col_names %||% setdiff(names(x), axes)

  stopifnot(
    !any(col_names %in% axes),
    !vec_duplicate_any(x[axes])
  )

  id <- expand.grid(dim_names,
                    KEEP.OUT.ATTRS = FALSE,
                    stringsAsFactors = FALSE)
  x <- vec_slice(x[col_names], vec_match(id, x[axes]))

  dim <- lengths(dim_names)
  x <- purrr::modify(as.list(x),
                     function(x) {
                       array(x, dim)
                     })
  new_dibble(x, dim_names)
}

is_dibble <- function(x) {
  inherits(x, "tbl_dim")
}

#' @export
as.list.tbl_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- as_list_dibble(x)
  purrr::modify(x,
                function(x) {
                  new_dim_col(x, dim_names)
                })
}

as_list_dibble <- function(x) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  attr(x, "group_names") <- NULL
  x
}

#' @export
dimnames.tbl_dim <- function(x) {
  dimnames_dibble(x)
}

dimnames_dibble <- function(x) {
  attr(x, "dim_names")
}

#' @export
`dimnames<-.tbl_dim` <- function(x, value) {
  assign_dimnames_dibble(x, value)
}

assign_dimnames_dibble <- function(x, value) {
  attr(x, "dim_names") <- as_dim_names(value, dimnames(x))
  x
}

#' @export
dim.tbl_dim <- function(x) {
  lengths(dimnames(x))
}

#' @export
nrow <- function(x) {
  UseMethod("nrow")
}
registerS3method("nrow", "default", base::nrow)

#' @export
nrow.tbl_dim <- function(x) {
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
ncol.tbl_dim <- function(x) {
  length(colnames(x))
}

#' @export
rownames <- function(x, ...) {
  UseMethod("rownames")
}
registerS3method("rownames", "default", base::rownames)

#' @export
rownames.tbl_dim <- function(x, ...) {
  NULL
}

#' @export
colnames <- function(x, ...) {
  UseMethod("colnames")
}
registerS3method("colnames", "default", base::colnames)

#' @export
colnames.tbl_dim <- function(x, ...) {
  names(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.tbl_dim <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

# aperm_tbl_dim <- function(a, perm, ...) {
#   dim_names <- dimnames(a)
#
#   perm <- vec_match(perm, names(dim_names))
#   dim_names <- dim_names[perm]
#
#   new_dibble(purrr::modify(as.list(a),
#                             function(x) {
#                               aperm(as.array(x),
#                                     perm = perm)
#                             }),
#               dim_names = dim_names)
# }



# Subsetting --------------------------------------------------------------

#' @export
`[.tbl_dim` <- function(x, i) {
  new_dibble(NextMethod(), dimnames(x))
}

#' @export
`[[.tbl_dim` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}

#' @export
`$.tbl_dim` <- function(x, i) {
  x <- as.list(x)
  x[[i]]
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.tbl_dim <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr mutate
#' @export
mutate.tbl_dim <- function(.data, ...) {
  dots <- rlang::enquos(..., .named = TRUE)
  nms <- names(dots)

  dim_names <- dimnames(.data)
  data <- as.list(.data)

  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    data[[nm]] <- as_dim_col(rlang::eval_tidy(dots[[i]], data), dim_names)
    .data[[nm]] <- as.array(data[[nm]])
  }
  .data
}

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_dim <- function(x, ...) {
  x
}



# Printing ----------------------------------------------------------------

#' @export
print.tbl_dim <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}

print_dibble <- function(x, n) {
  groups <- names(attr(x, "group_names"))

  x_head <- ungroup(head_dibble(x, n))
  df <- as_tibble_dibble(x_head,
                         .pack = TRUE)
  df <- new_data_frame(df,
                       class = c("tbl_dim_impl", "tbl"))
  if (is_dibble(x) || is_grouped_dim(x)) {
    attr(df, "tbl_sum") <- c(`A dibble` = obj_sum(x))

    if (!is.null(groups)) {
      attr(df, "tbl_sum") <- c(attr(df, "tbl_sum"),
                               Groups = commas(groups))
    }
  } else if (is_dim_col(x)) {
    attr(df, "tbl_sum") <- c(`A column` = obj_sum(x))
  }
  attr(df, "rows_total") <- prod(dim(x))
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

#' @importFrom pillar obj_sum
#' @export
obj_sum.tbl_dim <- function(x) {
  obj_sum_dibble(x)
}

obj_sum_dibble <- function(x) {
  paste(dim_sum(x), big_mark(ncol(x)),
        sep = " x ")
}

#' @importFrom pillar tbl_format_setup
#' @export
tbl_format_setup.tbl_dim_impl <- function(x, width, ..., n, max_extra_cols, max_footer_lines) {
  setup <- NextMethod()

  setup$tbl_sum <-  attr(x, "tbl_sum")

  rows_total_old <- setup$rows_total
  rows_total_new <- attr(x, "rows_total")
  setup$rows_total <- rows_total_new
  setup$rows_missing <- rows_total_new - (rows_total_old - setup$rows_missing)
  setup
}
