Ops_dibble <- function(e1, e2) {
  if (is_tbl_ddf(e1)) {
    e1 <- as_ddf_col(e1)
    is_ddf_col_e1 <- TRUE
  } else {
    is_ddf_col_e1 <- is_ddf_col(e1)
  }

  class <- class(e1)
  if (is_missing(e2)) {
    new_dim_names <- dimnames(e1)
  } else {
    if (is_tbl_ddf(e2)) {
      e2 <- as_ddf_col(e2)
      is_ddf_col_e2 <- TRUE
    } else {
      is_ddf_col_e2 <- is_ddf_col(e2)
    }

    if (is_ddf_col_e1 && is_ddf_col_e2) {
      old_dim_names_e1 <- dimnames(e1)
      old_dim_names_e2 <- dimnames(e2)
      new_dim_names <- union_dim_names(list(old_dim_names_e1, old_dim_names_e2))

      brdcst_e1 <- broadcast_dim_names_warn(old_dim_names_e1, new_dim_names)
      brdcst_e2 <- broadcast_dim_names_warn(old_dim_names_e2, new_dim_names)

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

  new_ddf_col(NextMethod(), new_dim_names,
              class = class)
}

matrixOps_dibble <- function(e1, e2) {
  e1 <- as_ddf_col(e1)
  e2 <- as_ddf_col(e2)

  class <- class(e1)
  dim_names_e1 <- dimnames(e1)
  dim_names_e2 <- dimnames(e2)

  size_dim_names_e1 <- vec_size(dim_names_e1)
  size_dim_names_e2 <- vec_size(dim_names_e2)

  new_dim_name <- union_dim_names(list(dim_names_e1[size_dim_names_e1], dim_names_e2[1]))
  dim_names_e1[size_dim_names_e1] <- new_dim_name
  dim_names_e2[1] <- new_dim_name

  e1 <- broadcast(e1, dim_names_e1)
  e2 <- broadcast(e2, dim_names_e2)

  if (vec_size(dim_names_e1) == 1L) {
    e1 <- as.vector(e1)
    dim_names_e1 <- NULL
  } else {
    e1 <- as.matrix(e1)
    dim_names_e1 <- dim_names_e1[1L]
  }

  if (vec_size(dim_names_e2) == 1L) {
    e2 <- as.vector(e2)
    dim_names_e2 <- NULL
  } else {
    e2 <- as.matrix(e2)
    dim_names_e2 <- dim_names_e2[2L]
  }

  new_dim_names <- purrr::compact(c(dim_names_e1, dim_names_e2))

  out <- NextMethod()

  if (vec_is_empty(new_dim_names)) {
    as.vector(out)
  } else {
    dim(out) <- list_sizes_unnamed(new_dim_names)
    new_ddf_col(out, new_dim_names,
                class = class)
  }
}

methods_dibble <- function(x, ...) {
  x <- as_ddf_col(x)
  NextMethod()
}
