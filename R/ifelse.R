
#' @export
ifelse <- function(test, yes, no, ...) {
  UseMethod("ifelse")
}

#' @export
ifelse.default <- function(test, yes, no, ...) {
  base::ifelse(test, yes, no)
}

#' @export
ifelse.tbl_ddf <- function(test, yes, no, ...) {
  ifelse_dibble(test, yes, no)
}

#' @export
ifelse.grouped_ddf <- function(test, yes, no, ...) {
  ifelse_dibble(test, yes, no)
}

ifelse_dibble <- function(test, yes, no) {
  ifelse(as_ddf_col(test), yes, no)
}

#' @export
ifelse.ddf_col <- function(test, yes, no, ...) {
  dim_names <- union_dim_names(dimnames(test), dimnames(yes), dimnames(no))
  test <- as.array(broadcast(test, dim_names))
  yes <- as.array(broadcast(yes, dim_names))
  no <- as.array(broadcast(no, dim_names))

  new_ddf_col(ifelse(test, yes, no),
              dim_names)
}
