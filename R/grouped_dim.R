new_grouped_dim <- function(x, group_names) {
  structure(x,
            class = "grouped_dim",
            group_names = group_names)
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  dim_names <- dimnames(.data)

  .data <- as.list(.data)
  col_names <- names(.data)
  .data <- bind_arrays(.data)

  loc <- tidyselect::eval_select(rlang::expr(c(...)), dim_names)
  group_names <- dim_names[loc]
  dim_names <- dim_names[-loc]

  .data <- apply(.data, 1 + loc,
                 function(x) {
                   x <- apply(x, 1, as.array,
                              simplify = FALSE)
                   names(x) <- col_names
                   new_tbl_dim(x,
                               dim_names = dim_names)
                 },
                 simplify = FALSE)
  new_grouped_dim(array(.data,
                        dim = lengths(group_names)),
                  group_names = group_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  loc <- length(attr(x, "group_names"))

  x <- purrr::array_tree(as.array(x))
  x <- bind_arrays(x)
  x <- apply(x, loc + 1, as.array,
             simplify = FALSE)
  new_tbl_dim(x,
              dim_names = dim_names)
}

is_grouped_dim <- function(x) {
  inherits(x, "grouped_dim")
}

#' @export
as.array.grouped_dim <- function(x, ...) {
  structure(x,
            class = NULL,
            group_names = NULL)
}

#' @export
dimnames.grouped_dim <- function(x) {
  c(attr(x, "group_names"), dimnames(x[[1]][[1]]))
}

#' @export
`dimnames<-.grouped_dim` <- function(x, value) {
  # TODO
  stop()
}

#' @export
dim.grouped_dim <- function(x) {
  lengths(dimnames(x))
}

#' @export
`dim<-.grouped_dim` <- function(x, value) {
  # TODO: add an error message
  stop()
}

#' @export
nrow.grouped_dim <- function(x) {
  nrow_dibble(x)
}

#' @export
ncol.grouped_dim <- function(x) {
  ncol(x[[1]])
}

#' @export
rownames.grouped_dim <- function(x, ...) {
  NULL
}

#' @export
colnames.grouped_dim <- function(x, ...) {
  names(x[[1]])
}



# Printing ----------------------------------------------------------------

#' @export
print.grouped_dim <- function(x, n = NULL, ...) {
  print_dibble(ungroup(x), n,
               groups = names(attr(x, "group_names")))
}

# obj_sum.grouped_dim <- function(x) {
#   paste(obj_sum(x[[1]]), big_mark(length(as.list(x))),
#         sep = " x ")
# }
