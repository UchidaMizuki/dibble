#' @include ddf_col.R tbl_ddf.R grouped_ddf.R
NULL

setOldClass("ddf_col")
setOldClass("tbl_ddf")
setOldClass("grouped_ddf")
setClassUnion("dibble", c("ddf_col", "tbl_ddf", "grouped_ddf", "vector"))

setGeneric("pmax",
           signature = "...")
setMethod("pmax", "vector", base::pmax)
setMethod("pmax", "dibble",
          function(..., na.rm = FALSE) {
            dibble_extremes(pmax, ...,
                            na.rm = na.rm)
          })

setGeneric("pmin",
           signature = "...")
setMethod("pmin", "vector", base::pmin)
setMethod("pmin", "dibble",
          function(..., na.rm = FALSE) {
            dibble_extremes(pmax, ...,
                            na.rm = na.rm)
          })

dibble_extremes <- function(f, ..., na.rm) {
  dots <- rlang::list2(...)
  dim_names <- union_dim_names(!!!lapply(dots, dimnames))
  dots <- lapply(rlang::list2(...),
                 function(x) {
                   as.array(ddf_col(x, dim_names))
                 })
  new_ddf_col(rlang::exec(f, !!!dots,
                          na.rm = na.rm),
              dim_names)
}
