rows_upsert_dibble <- function(type = c("insert", "update", "patch", "upsert"),
                               x, y,
                               conflict = c("error", "ignore"),
                               unmatched = c("error", "ignore")) {
  type <- arg_match(type, c("insert", "update", "patch", "upsert"))
  conflict <- arg_match(conflict, c("error", "ignore"))
  unmatched <- arg_match(unmatched, c("error", "ignore"))

  dim_names_x <- dimnames(x)
  dim_names_y <- dimnames(y)
  new_dim_names <- union_dim_names(list(dim_names_x, dim_names_y))

  if (type == "insert") {
    if (conflict == "error") {
      stopifnot(
        purrr::every(intersect_dim_names(list(dim_names_x, dim_names_y)), is_empty)
      )
    }
  } else if (type %in% c("update", "patch")) {
    if (unmatched == "error") {
      stopifnot(
        identical(dim_names_x, new_dim_names)
      )
    } else {
      new_dim_names <- dim_names_x
    }
  }

  if (is_ddf_col(x) || is_ddf_col(y)) {
    is_ddf_col_new <- TRUE
    x <- as_ddf_col(x)
    y <- as_ddf_col(y)
  } else {
    stopifnot(
      names(y) %in% names(x)
    )
    is_ddf_col_new <- FALSE
  }

  # locations of x to new
  brdcst_x <- broadcast_dim_names_warn(dim_names_x, new_dim_names)
  new <- broadcast_dibble(x, brdcst_x)

  loc_x <- brdcst_x$loc
  loc_x_in_new <- purrr::modify(loc_x, function(x) !is.na(x))
  loc_x_in_x <- purrr::modify(loc_x, function(x) vec_slice(x, !is.na(x)))
  brdcst_x$loc <- NULL

  # locations of y to new
  brdcst_y <- broadcast_dim_names_impl(dim_names_y, new_dim_names)

  loc_y <- brdcst_y$loc
  loc_y_in_new <- purrr::modify(loc_y, function(x) !is.na(x))
  loc_y_in_y <- purrr::modify(loc_y, function(x) vec_slice(x, !is.na(x)))
  brdcst_y$loc <- NULL

  y <- broadcast_dibble(y, brdcst_y)

  if (type == "insert") {
    x <- broadcast_dibble(x, brdcst_x)
  }

  if (is_ddf_col_new) {
    y <- exec(`[`, y, !!!loc_y_in_y,
              drop = FALSE)

    if (type == "insert") {
      x <- exec(`[`, x, !!!loc_x_in_x,
                drop = FALSE)

      new <- exec(`[<-`, new, !!!loc_y_in_new, y)
      new <- exec(`[<-`, new, !!!loc_x_in_new, x)
    } else if (type == "patch") {
      new_in_y <- exec(`[`, new, !!!loc_y_in_new,
                       drop = FALSE)

      loc_na <- is.na(new_in_y)
      new_in_y[loc_na] <- y[loc_na]

      new <- exec(`[<-`, new, !!!loc_y_in_new, new_in_y)
    } else {
      new <- exec(`[<-`, new, !!!loc_y_in_new, y)
    }
  } else {
    for (nm in names(y)) {
      y_nm <- exec(`[`, y[[nm]], !!!loc_y_in_y,
                   drop = FALSE)

      if (type == "insert") {
        x_nm <- exec(`[`, x[[nm]], !!!loc_x_in_x,
                     drop = FALSE)

        new[[nm]] <- exec(`[<-`, new[[nm]], !!!loc_y_in_new, y_nm)
        new[[nm]] <- exec(`[<-`, new[[nm]], !!!loc_x_in_new, x_nm)
      } else if (type == "patch") {
        new_in_y <- exec(`[`, new[[nm]], !!!loc_y_in_new,
                         drop = FALSE)

        loc_na <- is.na(new_in_y)
        new_in_y[loc_na] <- y_nm[loc_na]

        new[[nm]] <- exec(`[<-`, new[[nm]], !!!loc_y_in_new, new_in_y)
      } else {
        new[[nm]] <- exec(`[<-`, new[[nm]], !!!loc_y_in_new, y_nm)
      }
    }
  }

  if (is_ddf_col_new) {
    new_ddf_col(new, new_dim_names)
  } else {
    new_tbl_ddf(new, new_dim_names)
  }
}

#' @importFrom dplyr rows_insert
#' @export
rows_insert.ddf_col <- function(x, y,
                                by = NULL, ...,
                                conflict = c("error", "ignore"),
                                copy = FALSE,
                                in_place = FALSE) {
  rows_upsert_dibble("insert", x, y,
                     conflict = conflict)
}

#' @export
rows_insert.tbl_ddf <- function(x, y,
                                by = NULL, ...,
                                conflict = c("error", "ignore"),
                                copy = FALSE,
                                in_place = FALSE) {
  rows_upsert_dibble("insert", x, y,
                     conflict = conflict)
}

#' @importFrom dplyr rows_update
#' @export
rows_update.ddf_col <- function(x, y, by = NULL, ...,
                                unmatched = c("error", "ignore"),
                                copy = FALSE,
                                in_place = FALSE) {
  rows_upsert_dibble("update", x, y,
                     unmatched = unmatched)
}

#' @export
rows_update.tbl_ddf <- function(x, y, by = NULL, ...,
                                unmatched = c("error", "ignore"),
                                copy = FALSE,
                                in_place = FALSE) {
  rows_upsert_dibble("update", x, y,
                     unmatched = unmatched)
}

#' @importFrom dplyr rows_patch
#' @export
rows_patch.ddf_col <- function(x, y, by = NULL, ...,
                               unmatched = c("error", "ignore"),
                               copy = FALSE,
                               in_place = FALSE) {
  rows_upsert_dibble("patch", x, y,
                     unmatched = unmatched)
}

#' @export
rows_patch.tbl_ddf <- function(x, y, by = NULL, ...,
                               unmatched = c("error", "ignore"),
                               copy = FALSE,
                               in_place = FALSE) {
  rows_upsert_dibble("patch", x, y,
                     unmatched = unmatched)
}

#' @importFrom dplyr rows_upsert
#' @export
rows_upsert.ddf_col <- function(x, y, by = NULL, ...,
                                copy = FALSE,
                                in_place = FALSE) {
  rows_upsert_dibble("upsert", x, y)
}

#' @export
rows_upsert.tbl_ddf <- function(x, y, by = NULL, ...,
                                copy = FALSE,
                                in_place = FALSE) {
  rows_upsert_dibble("upsert", x, y)
}
