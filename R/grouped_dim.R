new_grouped_dim <- function(x, group_names) {
  structure(x,
            class = "grouped_dim",
            group_names = group_names)
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  dim_names <- dimnames(.data)

  loc <- tidyselect::eval_select(rlang::expr(c(...)), dim_names)
  margin <- length(dim_names) - loc + 1

  group_names <- dim_names[loc]
  dim_names <- dim_names[-loc]

  dim <- rev(lengths(group_names))
  .data <- purrr::modify(as.list(.data),
                         function(x) {
                           x <- apply(x, margin, as.array,
                                      simplify = FALSE)
                           array(x,
                                 dim = dim)
                         })

  len <- prod(dim)
  out <- vector("list", len)
  for (i in seq_len(len)) {
    out[[i]] <- new_tbl_dim(purrr::modify(.data,
                                          function(x) x[[i]]),
                            dim_names = dim_names)
  }

  new_grouped_dim(array(out,
                        dim = rev(lengths(group_names))),
                  group_names = group_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  col_names <- names(x[[1]])

  len <- length(col_names)
  out <- vector("list", len)
  names(out) <- col_names
  for (i in seq_len(len)) {
    out[[i]] <- abind::abind(purrr::modify(as.list(x),
                                           function(x) {
                                             as.array(x[[i]])
                                           }),
                             rev.along = 0)
    dim(out[[i]]) <- rev(dim(x))
  }

  new_tbl_dim(out,
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
dim.grouped_dim <- function(x) {
  lengths(dimnames(x))
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
