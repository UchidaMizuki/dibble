Ops_dibble <- function(e1, e2) {
  if (is_dibble(e1) || is_grouped_dibble(e1)) {
    e1 <- as_dibble_measure(e1)
    is_dibble_measure_e1 <- TRUE
  } else {
    is_dibble_measure_e1 <- is_dibble_measure(e1)
  }

  if (is_dibble(e2) || is_grouped_dibble(e2)) {
    e2 <- as_dibble_measure(e2)
    is_dibble_measure_e2 <- TRUE
  } else {
    is_dibble_measure_e2 <- is_dibble_measure(e2)
  }

  if (is_dibble_measure_e1 && is_dibble_measure_e2) {
    old_dim_names_e1 <- dimnames(e1)
    old_dim_names_e2 <- dimnames(e2)
    new_dim_names <- union_dim_names(old_dim_names_e1, old_dim_names_e2)

    brdcst_e1 <- broadcast_dim_names(old_dim_names_e1, new_dim_names)
    brdcst_e2 <- broadcast_dim_names(old_dim_names_e2, new_dim_names)

    e1 <- broadcast_array(as.array(e1), brdcst_e1)
    e2 <- broadcast_array(as.array(e2), brdcst_e2)
  } else if (is_dibble_measure_e1) {
    new_dim_names <- dimnames(e1)
    e1 <- as.array(e1)
  } else {
    new_dim_names <- dimnames(e2)
    e2 <- as.array(e2)
  }

  new_dibble_measure(NextMethod(), new_dim_names)
}

Math_dibble <- function(x) {
  x <- as_dibble_measure(x)
  NextMethod()
}
