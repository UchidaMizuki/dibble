new_tbl_dim <- function(x, dim_names) {
  structure(x,
            class = "tbl_dim",
            dim_names = dim_names)
}

dibble <- function() {
  # TODO
}

#' @export
as_dibble <- function(x, ...) {
  UseMethod("as_dibble")
}

#' @export
as_dibble.data.frame <- function(x, dim_names, cols = NULL, ...) {
  ellipsis::check_dots_empty()

  dim_names <- as_dim_names(dim_names)

  nms <- names(x)
  axes <- names(dim_names)
  cols <- cols %||% setdiff(nms, axes)

  stopifnot(
    axes %in% nms,
    !vctrs::vec_duplicate_any(x[axes])
  )

  dim_names <- purrr::modify2(dim_names, axes,
                              function(dim, axis) {
                                dim %||% unique(x[[axis]])
                              })
  names(dim_names) <- axes
  x[axes] <- purrr::modify2(x[axes], dim_names, vctrs::vec_match)

  ids <- expand.grid(purrr::modify(dim_names, seq_along),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = axes)

  dim <- lengths(dim_names)
  new_tbl_dim(purrr::modify(as.list(x[cols]),
                            function(x) {
                              array(as.double(x),
                                    dim = dim)
                            }),
              dim_names = dim_names)
}

is_tbl_dim <- function(x) {
  inherits(x, "tbl_dim")
}

#' @export
as.list.tbl_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- structure(x,
                 class = NULL,
                 dim_names = NULL)
  purrr::modify(x,
                function(x) {
                  new_dim_col(x,
                              dim_names = dim_names)
                })
}

#' @export
dimnames.tbl_dim <- function(x) {
  attr(x, "dim_names")
}

#' @export
`dimnames<-.tbl_dim` <- function(x, value) {
  attr(x, "dim_names") <- as_dim_names(value)
}

#' @export
dim.tbl_dim <- function(x) {
  lengths(dimnames(x))
}

#' @export
`dim<-.tbl_dim` <- function(x, value) {
  # TODO: add an error message
  stop()
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
  length(as.list(x))
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

aperm_tbl_dim <- function(a, perm, ...) {
  dim_names <- dimnames(a)

  perm <- vctrs::vec_match(perm, names(dim_names))
  dim_names <- dim_names[perm]

  new_tbl_dim(purrr::modify(as.list(a),
                            function(x) {
                              aperm(as.array(x),
                                    perm = perm)
                            }),
              dim_names = dim_names)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.tbl_dim` <- function(x, ...) {
  structure(NextMethod(),
            class = "tbl_dim",
            dim_names = dimnames(x))
}

#' @export
`[<-.tbl_dim` <- function(x, ...) {
  # TODO
  stop()
}

#' @export
`[[.tbl_dim` <- function(x, ...) {
  x <- as.list(x)
  NextMethod()
}

#' @export
`[[<-.tbl_dim` <- function(x, ...) {
  # TODO
  stop()
}

#' @export
`$.tbl_dim` <- function(x, ...) {
  x <- as.list(x)
  NextMethod()
}

#' @export
`$<-.tbl_dim` <- function(x, i) {
  # TODO
  stop()
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.tbl_dim <- function(.data, ...) {
  slice_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.tbl_dim <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}

print_dibble <- function(x, n,
                         groups = NULL) {
  df <- as_tibble_dibble(head_dibble(x, n),
                         .pack = TRUE)
  df <- vctrs::new_data_frame(df,
                              class = c("tbl_dim_impl", "tbl"))
  if (is_tbl_dim(x)) {
    attr(df, "tbl_sum") <- c(`A dimensional tibble` = obj_sum(x))

    if (!is.null(groups)) {
      attr(df, "tbl_sum") <- c(attr(df, "tbl_sum"),
                               Groups = commas(groups))
    }
  } else if (is_dim_col(x)) {
    attr(df, "tbl_sum") <- c(`A dimensional column` = obj_sum(x))
  }
  attr(df, "rows_total") <- prod(dim(x))
  print(df)

  invisible(x)
}

head_dibble <- function(x, n) {
  # n <- n %||% pillar:::get_pillar_option_print_max() + 1
  n <- n %||% 21
  dim <- dim(x)

  loc <- rep(1, length(dim))
  i <- cumprod(dim) < n
  dim <- dim[i]

  loc[i] <- dim

  if (!all(i)) {
    loc[[which(!i)[[1]]]] <- ceiling(n / prod(dim))
  }
  loc <- purrr::map(loc, seq_len)

  slice_dibble(x, !!!loc)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.tbl_dim <- function(x) {
  paste(obj_sum(x[[1]]), big_mark(ncol(x)),
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
