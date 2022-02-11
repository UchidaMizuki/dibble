setOldClass("ddf_col")
setOldClass("tbl_ddf")
setOldClass("grouped_ddf")

setClassUnion("dibble", c("ddf_col", "tbl_ddf", "grouped_ddf", "array", "vector"))

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
  dots <- list2(...)
  dim_names <- union_dim_names(!!!lapply(dots, dimnames))
  dots <- lapply(list2(...),
                 function(x) {
                   as.array(broadcast(x, dim_names))
                 })
  new_ddf_col(exec(f, !!!dots,
                   na.rm = na.rm),
              dim_names)
}
