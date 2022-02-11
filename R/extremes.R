#' @include S4.R
NULL

#' @export
setGeneric("pmax",
           signature = "...")

#' @export
setMethod("pmax", "dibble",
          function(..., na.rm = FALSE) {
            dibble_extremes(base::pmax, ...,
                            na.rm = na.rm)
          })

#' @export
setGeneric("pmin",
           signature = "...")

#' @export
setMethod("pmin", "dibble",
          function(..., na.rm = FALSE) {
            dibble_extremes(base::pmin, ...,
                            na.rm = na.rm)
          })

dibble_extremes <- function(f, ..., na.rm) {
  dots <- rlang::list2(...)
  dim_names <- union_dim_names(!!!lapply(dots, dimnames))
  dots <- lapply(rlang::list2(...),
                 function(x) {
                   as.array(broadcast(x, dim_names))
                 })
  new_ddf_col(rlang::exec(f, !!!dots,
                          na.rm = na.rm),
              dim_names)
}
