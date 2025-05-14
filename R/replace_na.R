#' @importFrom tidyr replace_na
#' @export
replace_na.tbl_ddf <- function(data, replace, ...) {
  if (!vec_is_list(replace)) {
    abort("`replace` must be a list.")
  }
  if (!is_named(replace)) {
    abort("`replace` must be a named list.")
  }

  nms_data <- names(data)
  nms_replace <- names(replace)

  for (i in vec_seq_along(replace)) {
    nm_replace <- nms_replace[[i]]

    if (nm_replace %in% nms_data) {
      data_replaced <- replace_na(data[[nm_replace]], replace[[i]])
      data[[nm_replace]] <- undibble(data_replaced)
    }
  }
  data
}

#' @export
replace_na.ddf_col <- function(data, replace, ...) {
  ifelse(is.na(data), replace, data)
}
