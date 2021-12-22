slice_dibble <- function(.data, ...) {
  dots <- lapply(rlang::list2(...),
                 function(x) {
                   x %||% rlang::missing_arg()
                 })
  nms <- rlang::names2(dots)

  dim_names <- dimnames(.data)
  axes <- names(dim_names)

  stopifnot(
    length(dots) == length(dim_names),
    nms == "" | nms %in% axes
  )

  names(dots)[nms == ""] <- axes[!axes %in% nms]
  dots <- dots[axes]
  dim_names <- mapply(`[`, dim_names, dots,
                      SIMPLIFY = FALSE)

  if (is_dibble(.data)) {
    new_dibble(lapply(.data, function(x) rlang::exec(`[`, x, !!!dots)),
               dim_names = dim_names)
  } else if (is_dim_col(.data)) {
    new_dim_col(rlang::exec(`[`, .data, !!!dots),
                dim_names = dim_names)
  }
}
