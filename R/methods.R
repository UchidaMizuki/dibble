Ops_dibble <- function(e1, e2) {
  if (is_tbl_ddf(e1) || is_grouped_ddf(e1)) {
    e1 <- as_ddf_col(e1)
    is_ddf_col_e1 <- TRUE
  } else {
    is_ddf_col_e1 <- is_ddf_col(e1)
  }

  if (is_missing(e2)) {
    new_dim_names <- dimnames(e1)
  } else {
    if (is_tbl_ddf(e2) || is_grouped_ddf(e2)) {
      e2 <- as_ddf_col(e2)
      is_ddf_col_e2 <- TRUE
    } else {
      is_ddf_col_e2 <- is_ddf_col(e2)
    }

    if (is_ddf_col_e1 && is_ddf_col_e2) {
      old_dim_names_e1 <- dimnames(e1)
      old_dim_names_e2 <- dimnames(e2)
      new_dim_names <- union_dim_names(old_dim_names_e1, old_dim_names_e2)

      brdcst_e1 <- broadcast_dim_names(old_dim_names_e1, new_dim_names)
      brdcst_e2 <- broadcast_dim_names(old_dim_names_e2, new_dim_names)

      e1 <- broadcast_array(as.array(e1), brdcst_e1)
      e2 <- broadcast_array(as.array(e2), brdcst_e2)
    } else if (is_ddf_col_e1) {
      new_dim_names <- dimnames(e1)
      e1 <- as.array(e1)
    } else {
      new_dim_names <- dimnames(e2)
      e2 <- as.array(e2)
    }
  }

  new_ddf_col(NextMethod(), new_dim_names)
}

methods_dibble <- function(x, ...) {
  x <- as_ddf_col(x)
  NextMethod()
}
