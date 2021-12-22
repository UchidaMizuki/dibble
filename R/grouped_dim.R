# TODO
new_grouped_dim <- function(x, dim_names) {
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
            class = "grouped_dim",
            dim_names = dim_names)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_dim <- function(x) {
  names(dimnames(x))
}

#' @importFrom dplyr group_vars
#' @export
group_vars.tbl_dim <- function(x) {
  # TODO
  names(dimnames(x)[attr(x, "groups")])
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  group_names <- dplyr::group_by_prepare(.data, ...)$group_names

  dim_names <- dimnames(.data)
  axes <- names(dim_names)
  margin <- vctrs::vec_match(group_names, axes)

  x <- lapply(.data,
              function(x) {
                apply(x, margin,
                      FUN = function(x) {
                        new_dim_col(x,
                                    dim_names = dim_names[-margin])
                      },
                      simplify = FALSE)
              })
  new_dibble(x,
             dim_names = dim_names[margin])
}
