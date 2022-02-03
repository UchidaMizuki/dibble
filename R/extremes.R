
#' @export
pmax <- function(..., na.rm = FALSE) {
  UseMethod("pmax")
}

#' @export
pmax.default <- function(..., na.rm = FALSE) {
  base::pmax(...,
             na.rm = na.rm)
}

#' @export
pmax.dibble <- function(..., na.rm = FALSE) {
  pmax_dibble(...,
              na.rm = na.rm)
}

#' @export
pmax.grouped_dibble <- function(..., na.rm = FALSE) {
  pmax_dibble(...,
              na.rm = na.rm)
}

#' @export
pmax.dibble_measure <- function(..., na.rm = FALSE) {
  pmax_dibble(...,
              na.rm = na.rm)
}

pmax_dibble <- function(..., na.rm = FALSE) {
  dots <- rlang::list2(...)
  dim_names <- union_dim_names(!!!lapply(dots, dimnames))
  dots <- lapply(rlang::list2(...),
                 function(x) {
                   # FIXME?
                   as.array(dibble_measure(x, dim_names))
                 })
  new_dibble_measure(rlang::exec(pmax, !!!dots,
                                 na.rm = na.rm),
                     dim_names)
}

#' @export
pmin <- function(..., na.rm = FALSE) {
  UseMethod("pmin")
}

#' @export
pmin.default <- function(..., na.rm = FALSE) {
  base::pmin(...,
             na.rm = na.rm)
}

#' @export
pmin.dibble <- function(..., na.rm = FALSE) {
  pmin_dibble(...,
              na.rm = na.rm)
}

#' @export
pmin.grouped_dibble <- function(..., na.rm = FALSE) {
  pmin_dibble(...,
              na.rm = na.rm)
}

#' @export
pmin.dibble_measure <- function(..., na.rm = FALSE) {
  pmin_dibble(...,
              na.rm = na.rm)
}

pmin_dibble <- function(..., na.rm = FALSE) {
  dots <- rlang::list2(...)
  dim_names <- union_dim_names(!!!lapply(dots, dimnames))
  dots <- lapply(rlang::list2(...),
                 function(x) {
                   # FIXME?
                   as.array(dibble_measure(x, dim_names))
                 })
  new_dibble_measure(rlang::exec(pmin, !!!dots,
                                 na.rm = na.rm),
                     dim_names)
}
