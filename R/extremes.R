#' Maxima and Minima
#'
#' Returns the parallel maxima and minima of the input values.
#'
#' These functions override base functions to make them generic. The default
#' methods call the base versions.
#'
#' @param ... Dibbles, numeric or character arguments.
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @return A dibble if `...` are dibbles. See [base::pmax()] and [base::pmin()]
#' for the return value of the default method.
#'
#' @seealso [base::pmax()], [base::pmin()].
#'
#' @name extremes
NULL

#' @rdname extremes
#' @export
pmax <- function(...,
                 na.rm = FALSE) {
  UseMethod("pmax")
}

#' @rdname extremes
#' @export
pmax.default <- function(...,
                         na.rm = FALSE) {
  base::pmax(...,
             na.rm = na.rm)
}

#' @rdname extremes
#' @export
pmax.ddf_col <- function(...,
                         na.rm = FALSE) {
  pmax_dibble(...,
              na.rm = na.rm)
}

#' @rdname extremes
#' @export
pmax.tbl_ddf <- function(...,
                         na.rm = FALSE) {
  pmax_dibble(...,
              na.rm = na.rm)
}

pmax_dibble <- function(..., na.rm) {
  args <- list2(...)
  dim_names <- union_dim_names(map(args, dimnames))
  args <- map(args,
              function(x) {
                as.array(broadcast(x, dim_names))
              })

  new_ddf_col(exec(base::pmax, !!!args),
              dim_names)
}

#' @rdname extremes
#' @export
pmin <- function(...,
                 na.rm = FALSE) {
  UseMethod("pmin")
}

#' @rdname extremes
#' @export
pmin.default <- function(...,
                         na.rm = FALSE) {
  base::pmin(...,
             na.rm = na.rm)
}

#' @rdname extremes
#' @export
pmin.ddf_col <- function(...,
                         na.rm = FALSE) {
  pmin_dibble(...,
              na.rm = na.rm)
}

#' @rdname extremes
#' @export
pmin.tbl_ddf <- function(...,
                         na.rm = FALSE) {
  pmin_dibble(...,
              na.rm = na.rm)
}

pmin_dibble <- function(..., na.rm) {
  args <- list2(...)
  dim_names <- union_dim_names(map(args, dimnames))
  args <- map(args,
              function(x) {
                as.array(broadcast(x, dim_names))
              })

  new_ddf_col(exec(base::pmin, !!!args),
              dim_names)
}
