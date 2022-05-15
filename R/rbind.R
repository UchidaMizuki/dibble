#' @export
rbind.ddf_col <- function(..., deparse.level = 1) {
  rbind_dibble(...)
}

#' @export
rbind.tbl_ddf <- function(..., deparse.level = 1) {
  rbind_dibble(...)
}

rbind_dibble <- function(...) {
  args <- list2(...)
  purrr::reduce(args, rows_insert)
}
