new_grouped_dim <- function(x, dim_names, group_names) {
  dim <- lengths(dim_names,
                 use.names = FALSE)
  size <- prod(dim)
  x <- purrr::modify(as.list(x),
                     function(x) {
                       array(vctrs::vec_recycle(as.vector(x), size),
                             dim = lengths(dim_names))
                     })
  structure(x,
            class = "grouped_dim",
            dim_names = dim_names,
            group_names = group_names)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_dim <- function(x) {
  names(dimnames(x))
}

#' @importFrom dplyr group_vars
#' @export
group_vars.tbl_dim <- function(x) {
  names(dimnames(x)[attr(x, "groups")])
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  .data <- as.list(.data)
  nms <- names(.data)
  .data <- abind::abind(unname(.data),
                        along = length(axes) + 1)

  loc <- tidyselect::eval_select(rlang::expr(c(...)), dim_names)
  group_dim_names <- dim_names[loc]
  dim_names <- dim_names[-loc]

  .data <- apply(.data, loc,
                 function(x) {
                   x <- apply(x, length(dim(x)),
                              function(x){
                                x
                              },
                              simplify = FALSE)
                   names(x) <- nms
                   new_tbl_dim(x,
                               dim_names = dim_names)
                 },
                 simplify = FALSE)
  array(.data,
        dim = lengths(group_dim_names),
        dimnames = group_dim_names)

  # data <- purrr::modify(as.list(.data),
  #                       function(x) {
  #                         apply(x, loc,
  #                               function(x) {
  #                                 new_dim_col(x,
  #                                             dim_names = dim_names)
  #                               },
  #                               simplify = FALSE)
  #                       })
  # new_grouped_dim(data,
  #                 dim_names = group_dim_names,
  #                 group_names = axes[loc])
}

is_grouped_dim <- function(x) {
  inherits(x, "grouped_dim")
}

#' @export
dimnames.grouped_dim <- function(x) {
  c(attr(x, "dim_names"), dimnames(x[[1]][[1]]))
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
