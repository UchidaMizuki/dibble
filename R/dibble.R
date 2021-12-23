new_tbl_dim <- function(x, dim_names) {
  dim <- lengths(dim_names,
                 use.names = FALSE)
  size <- prod(dim)
  x <- purrr::modify(as.list(x),
                     function(x) {
                       array(vctrs::vec_recycle(as.vector(x),
                                                size = size),
                             dim = dim)
                     })
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
  x[axes] <- purrr::modify2(x[axes], dim_names, vctrs::vec_match)

  ids <- expand.grid(purrr::modify(dim_names, seq_along),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = axes)
  new_tbl_dim(x[cols],
              dim_names = dim_names)
}

#' @export
is_tbl_dim <- function(x) {
  inherits(x, "tbl_dim")
}

#' @export
as.list.tbl_dim <- function(x, ...) {
  structure(x,
            class = NULL,
            dim_names = NULL)
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

#' @importFrom tibble as_tibble
#' @export
as_tibble.tbl_dim <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

#' @export
aperm.tbl_dim <- function(a, perm, ...) {
  dim_names <- dimnames(a)

  perm <- vctrs::vec_match(perm, names(dim_names))
  dim_names <- dim_names[perm]

  new_tbl_dim(purrr::modify(a,
                            function(x) {
                              aperm(as.array(x),
                                    perm = perm)
                            }),
              dim_names = dim_names)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.tbl_dim` <- function(x, i) {
  structure(NextMethod(),
            class = "tbl_dim",
            dim_names = dimnames(x))
}

#' @export
`[<-.tbl_dim` <- function(x, i, value) {

}

#' @export
`[[.tbl_dim` <- function(x, i) {
  new_dim_col(NextMethod(),
              dim_names = dimnames(x))
}

#' @export
`$.tbl_dim` <- function(x, i) {
  new_dim_col(NextMethod(),
              dim_names = dimnames(x))
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
  df <- vctrs::new_data_frame(as_tibble_dibble(head_dibble(x, n),
                                               .pack = TRUE),
                              class = c("tbl_dim_impl", "tbl"))
  attr(df, "tbl_sum") <- c(`A dimensional tibble` = obj_sum(x))
  attr(df, "rows_total") <- prod(dim(x))
  print(df)

  invisible(x)
}

head_dibble <- function(x, n) {
  # n <- n %||% pillar:::get_pillar_option_print_max() + 1
  n <- n %||% 21
  dim <- lengths(dimnames(x),
                 use.names = FALSE)

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
  paste(obj_sum(x[[1]]), big_mark(length(as.list(x))),
        sep = " x ")
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.tbl_dim <- function(x, ...) {
  "dibble"
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
