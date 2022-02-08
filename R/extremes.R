#' @include dibble.R grouped_dibble.R
NULL

setOldClass("dibble")
setOldClass("grouped_dibble")
setOldClass("dibble_measure")
setClassUnion("Dibble", c("dibble", "grouped_dibble", "dibble_measure", "numeric"))

setGeneric("pmax",
           signature = "...")
setMethod("pmax", "Dibble",
          function(..., na.rm = FALSE) {
            dibble_extremes(pmax, ...,
                            na.rm = na.rm)
          })

setGeneric("pmin",
           signature = "...")
setMethod("pmin", "Dibble",
          function(..., na.rm = FALSE) {
            dibble_extremes(pmin, ...,
                            na.rm = na.rm)
          })

dibble_extremes <- function(f, ..., na.rm) {
  dots <- rlang::list2(...)
  dim_names <- union_dim_names(!!!lapply(dots, dimnames))
  dots <- lapply(rlang::list2(...),
                 function(x) {
                   # FIXME?
                   as.array(dibble_measure(x, dim_names))
                 })
  new_dibble_measure(rlang::exec(f, !!!dots,
                                 na.rm = na.rm),
                     dim_names)
}
