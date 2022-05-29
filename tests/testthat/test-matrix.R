test_that("%*%", {
  # mat %*% mat
  mat_x <- matrix(1:9, 3,
                  dimnames = list(axis1 = 1:3,
                                  axis2 = 1:3))
  mat_y <- matrix(1:9, 3,
                  dimnames = list(axis2 = 1:3,
                                  axis3 = 1:3))
  ddf_x <- as_dibble(mat_x)
  ddf_y <- as_dibble(mat_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(mat_x %*% mat_y))

  mat_x <- matrix(1:9, 3,
                  dimnames = list(axis2 = 1:3,
                                  axis1 = 1:3))
  mat_y <- matrix(1:9, 3,
                  dimnames = list(axis2 = 1:3,
                                  axis3 = 1:3))
  ddf_x <- as_dibble(mat_x)
  ddf_y <- as_dibble(mat_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(t(mat_x) %*% mat_y))

  # vec %*% mat
  vec_x <- array(1:3, 3,
                 dimnames = list(axis2 = 1:3))
  mat_y <- matrix(1:9, 3,
                  dimnames = list(axis2 = 1:3,
                                  axis3 = 1:3))
  ddf_x <- as_dibble(vec_x)
  ddf_y <- as_dibble(mat_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), t(unname(vec_x %*% mat_y)))

  vec_x <- array(1:3, 3,
                 dimnames = list(axis2 = 1:3))
  mat_y <- matrix(1:9, 3,
                  dimnames = list(axis3 = 1:3,
                                  axis2 = 1:3))
  ddf_x <- as_dibble(vec_x)
  ddf_y <- as_dibble(mat_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), t(unname(vec_x %*% t(mat_y))))

  # mat %*% vec
  mat_x <- matrix(1:9, 3,
                  dimnames = list(axis1 = 1:3,
                                  axis2 = 1:3))
  vec_y <- array(1:3, 3,
                 dimnames = list(axis2 = 1:3))
  ddf_x <- as_dibble(mat_x)
  ddf_y <- as_dibble(vec_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(mat_x %*% vec_y))

  mat_x <- matrix(1:9, 3,
                  dimnames = list(axis2 = 1:3,
                                  axis1 = 1:3))
  vec_y <- array(1:3, 3,
                 dimnames = list(axis2 = 1:3))
  ddf_x <- as_dibble(mat_x)
  ddf_y <- as_dibble(vec_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(t(mat_x) %*% vec_y))
})

test_that("t", {
  # vec
  vec <- array(1:3, 3,
               dimnames = list(axis = 1:3))
  ddf <- as_dibble(vec)
  expect_equal(as.array(t(ddf)), unname(t(vec)))

  # mat
  mat <- matrix(1:9, 3,
                dimnames = list(axis1 = 1:3,
                                axis2 = 1:3))
  ddf <- as_dibble(mat)
  expect_equal(as.array(t(ddf)), unname(t(mat)))
})

test_that("diag", {
  arr <- matrix(1:9, 3)
  ddf_col <- broadcast(1:9,
                       list(axis1 = letters[1:3],
                            axis2 = letters[1:3]))
  expect_equal(as.vector(diag(ddf_col, "axis")), diag(arr))

  arr_diag <- diag(arr)
  diag(arr) <- arr_diag + 1
  diag(ddf_col) <- arr_diag + 1

  expect_equal(as.array(ddf_col), arr)
})
