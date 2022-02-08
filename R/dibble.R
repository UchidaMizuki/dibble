#' Build a dimensional data frame
#'
#' `dibble()` constructs a dimensional data frame called a dibble.
#'
#' @param ... A set of name-measure pairs.
#' @param .dim_names A list of dimension names.
#'
#' @return A dibble.
#'
#' @export
dibble <- function(...,
                   .dim_names = NULL) {
  dots <- lapply(rlang::enquos(...),
                 function(x) {
                   suppress_warning_broadcast(
                     rlang::eval_tidy(x)
                   )
                 })

  dim_names <- union_dim_names(!!!lapply(unname(dots), dimnames))
  new_dim_names <- as_dim_names(.dim_names, dim_names)

  dots <- mapply(dots, rlang::names2(dots),
                 FUN = function(x, nm) {
                   if (is_dibble(x) || is_grouped_ddf(x)) {
                     x <- ungroup(x)
                     x <- lapply(as.list(x),
                                 function(x) {
                                   # FIXME?: When should we do `supress_warning`?
                                   if (!is.null(.dim_names)) {
                                     x <- suprpess_warning_broadcast(dibble_measure(x, dim_names))
                                   } else {
                                     x <- dibble_measure(x, dim_names)
                                   }
                                   undibble(x)
                                 })
                     if (nm != "") {
                       stopifnot(
                         rlang::is_scalar_list(x)
                       )
                       names(x) <- nm
                     }
                     x
                   } else {
                     old_dim_names <- dimnames(x)



                     stop()




                     # FIXME?: When should we do `supress_warning`?
                     if (!is.null(.dim_names)) {
                       x <- suppress_warning_broadcast(
                         dibble_measure(x, dim_names)
                       )
                     } else {
                       x <- dibble_measure(x, dim_names)
                     }
                     x <- list(undibble(x))
                     names(x) <- nm
                     x
                   }
                 },
                 SIMPLIFY = FALSE,
                 USE.NAMES = FALSE)
  dots <- vec_c(!!!dots)

  if (!rlang::is_named(dots)) {
    stopifnot(
      rlang::is_scalar_list(dots)
    )
    new_ddf_col(dots[[1L]], dim_names)
  } else {
    new_tbl_ddf(dots, dim_names)
  }
}

#' Constructs a dibble by one or more variables
#'
#' `dibble_by()` constructs a dibble by one or more variables.
#'
#' @param x A data frame or a dibble.
#' @param ... Variables.
#'
#' @export
dibble_by <- function(x, ...) {
  as_dibble(dplyr::rowwise(x, ...))
}

#' Coerce an object to a dibble
#'
#' `as_dibble()` turns an object into a dimensional data frame called a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#' @param ... Unused, for extensibility.
#'
#' @return A dibble.
#'
#' @export
as_dibble <- function(x, ...) {
  UseMethod("as_dibble")
}

#' @rdname as_dibble
#' @export
as_dibble.rowwise_df <- function(x, ...) {
  axes <- dplyr::group_vars(x)
  x <- ungroup(x)
  haystack <- x[axes]
  stopifnot(
    !vec_duplicate_any(haystack)
  )

  dim_names <- lapply(haystack, vec_unique)
  dim <- list_sizes(dim_names)

  needles <- expand.grid(dim_names,
                         KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = FALSE)
  x <- vec_slice(x[!names(x) %in% axes],
                 vec_match(needles, haystack))
  x <- lapply(x,
              function(x) {
                array(x, dim)
              })
  new_tbl_ddf(x, dim_names)
}

#' @rdname as_dibble
#' @export
as_dibble.grouped_df <- function(x, ...) {
  as_dibble.rowwise_df(x, ...)
}

#' @rdname as_dibble
#' @export
as_dibble.tbl_ddf <- function(x, ...) {
  x
}

#' @rdname as_dibble
#' @export
as_dibble.grouped_ddf <- function(x, ...) {
  ungroup(x)
}

#' @rdname as_dibble
#' @export
as_dibble.array <- function(x, ...) {
  as_dibble_measure(x, ...)
}

#' @rdname as_dibble
#' @export
as_dibble.table <- function(x, ...) {
  as_dibble_measure(x, ...)
}

#' Test if the object is a dibble
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble <- function(x) {
  is_ddf_col(x) || is_tbl_ddf(x) || is_grouped_ddf(x)
}

are_dibble <- function(x) {
  vapply(x, is_dibble,
         logical(1))
}

undibble <- function(x) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  attr(x, "group_dim_names") <- NULL
  x
}
