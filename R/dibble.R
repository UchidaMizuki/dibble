new_dibble <- function(x, dim_names) {
  dim <- lengths(dim_names, use.names = FALSE)
  size <- prod(dim)
  structure(lapply(x,
                   function(x) {
                     stopifnot(
                       length(x) == size
                     )
                     array(x,
                           dim = dim)
                   }),
            class = "tbl_dim",
            dim_names = dim_names)
}

#' @export
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

  dim_names <- mapply(dim_names, axes,
                      FUN = function(dim, axis) {
                        dim %||% unique(x[[axis]])
                      },
                      SIMPLIFY = FALSE)
  x[axes] <- mapply(x[axes], dim_names,
                    FUN = function(x, dim) {
                      vctrs::vec_match(x, dim)
                    },
                    SIMPLIFY = FALSE)

  ids <- expand.grid(lapply(dim_names, seq_along),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  x <- dplyr::left_join(ids, x,
                        by = axes)
  new_dibble(x[cols],
             dim_names = dim_names)
}

#' @export
is_dibble <- function(x) {
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

#' @importFrom tibble as_tibble
#' @export
as_tibble.tbl_dim <- function(x, ...) {
  dim <- expand.grid(dimnames(x),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  col <- lapply(x, as.vector)

  tibble::tibble(dim = as_tibble(dim),
                 col = as_tibble(col))
}



# Subsetting --------------------------------------------------------------

#' @export
`[.tbl_dim` <- function(x, ...) {
  structure(NextMethod(),
            class = "tbl_dim",
            dim_names = dimnames(x))
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
  out <- head_dibble(x, n)
  out <- vctrs::new_data_frame(as_tibble(out),
                               class = c("tbl_dim_impl", "tbl"))
  attr(out, "tbl_sum") <- c(`A dimensional tibble` = obj_sum(x))
  attr(out, "rows_total") <- prod(dim(x))
  print(out)

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
  loc <- lapply(loc, seq_len)

  slice(x, !!!loc)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.tbl_dim <- function(x) {
  paste(obj_sum(x[[1]]), paste0("col", pillar::size_sum(as.list(x))),
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
