test_that("`%*%`() works", {
  rev_axis <- function(x, axis) {
    dim_names <- dimnames(x)
    dim_names[[axis]] <- rev(dim_names[[axis]])
    broadcast(x, dim_names)
  }

  # mat %*% mat
  mat_x <- matrix(1:9, 3, dimnames = list(axis1 = 1:3, axis2 = 1:3))
  mat_y <- matrix(1:9, 3, dimnames = list(axis2 = 1:3, axis3 = 1:3))
  ddf_x <- as_dibble(mat_x)
  ddf_y <- as_dibble(mat_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(mat_x %*% mat_y))
  expect_equal(as.matrix(rev_axis(ddf_x, 2) %*% ddf_y), unname(mat_x %*% mat_y))
  expect_equal(as.matrix(ddf_x %*% rev_axis(ddf_y, 1)), unname(mat_x %*% mat_y))

  ddf_x <- dibble(x = ddf_x)
  ddf_y <- dibble(x = ddf_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(mat_x %*% mat_y))
  expect_equal(as.matrix(rev_axis(ddf_x, 2) %*% ddf_y), unname(mat_x %*% mat_y))
  expect_equal(as.matrix(ddf_x %*% rev_axis(ddf_y, 1)), unname(mat_x %*% mat_y))

  # vec %*% mat
  vec_x <- array(1:3, 3, dimnames = list(axis2 = 1:3))
  mat_y <- matrix(1:9, 3, dimnames = list(axis2 = 1:3, axis3 = 1:3))
  ddf_x <- as_dibble(vec_x)
  ddf_y <- as_dibble(mat_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), t(unname(vec_x %*% mat_y)))
  expect_equal(
    as.matrix(rev_axis(ddf_x, 1) %*% ddf_y),
    t(unname(vec_x %*% mat_y))
  )
  expect_equal(
    as.matrix(ddf_x %*% rev_axis(ddf_y, 1)),
    t(unname(vec_x %*% mat_y))
  )

  # mat %*% vec
  mat_x <- matrix(1:9, 3, dimnames = list(axis1 = 1:3, axis2 = 1:3))
  vec_y <- array(1:3, 3, dimnames = list(axis2 = 1:3))
  ddf_x <- as_dibble(mat_x)
  ddf_y <- as_dibble(vec_y)
  expect_equal(as.matrix(ddf_x %*% ddf_y), unname(mat_x %*% vec_y))
  expect_equal(as.matrix(rev_axis(ddf_x, 2) %*% ddf_y), unname(mat_x %*% vec_y))
  expect_equal(as.matrix(ddf_x %*% rev_axis(ddf_y, 1)), unname(mat_x %*% vec_y))

  # vec %*% vec
  vec_x <- array(1:3, 3, dimnames = list(axis2 = 1:3))
  vec_y <- array(1:3, 3, dimnames = list(axis2 = 1:3))
  ddf_x <- as_dibble(vec_x)
  ddf_y <- as_dibble(vec_y)
  expect_equal(ddf_x %*% ddf_y, as.vector(vec_x %*% vec_y))
  expect_equal(rev_axis(ddf_x, 1) %*% ddf_y, as.vector(vec_x %*% vec_y))
  expect_equal(ddf_x %*% rev_axis(ddf_y, 1), as.vector(vec_x %*% vec_y))
})

test_that("t() works", {
  # vec
  vec <- array(1:3, 3, dimnames = list(axis = 1:3))
  ddf <- as_dibble(vec)
  expect_equal(as.array(t(ddf)), unname(t(vec)))

  ddf <- dibble(x = ddf)
  expect_equal(as.array(t(ddf)), unname(t(vec)))

  # mat
  mat <- matrix(1:9, 3, dimnames = list(axis1 = 1:3, axis2 = 1:3))
  ddf <- as_dibble(mat)
  expect_equal(as.array(t(ddf)), unname(t(mat)))
})

test_that("solve() works", {
  set.seed(1234)

  # mat
  mat <- matrix(runif(9), 3, dimnames = list(axis1 = 1:3, axis2 = 1:3))
  ddf <- as_dibble(mat)
  expect_equal(as.matrix(solve(ddf)), unname(solve(mat)))

  ddf <- dibble(x = ddf)
  expect_equal(as.matrix(solve(ddf)), unname(solve(mat)))
})

test_that("diag() works", {
  arr <- matrix(1:9, 3)
  ddf_col <- broadcast(1:9, list(axis1 = letters[1:3], axis2 = letters[1:3]))
  expect_equal(as.vector(diag(ddf_col, "axis")), diag(arr))

  arr_diag <- diag(arr)
  diag(arr) <- arr_diag + 1
  diag(ddf_col) <- arr_diag + 1

  expect_equal(as.array(ddf_col), arr)
})

test_that("eye(), ones() and zeros() work", {
  ddf <- broadcast(1:9, list(axis1 = letters[1:3], axis2 = letters[1:3]))
  expect_equal(as.matrix(eye(ddf)), diag(3))
  expect_equal(as.matrix(ones(ddf)), matrix(1, 3, 3))
  expect_equal(as.matrix(zeros(ddf)), matrix(0, 3, 3))

  ddf <- dibble(x = ddf)
  expect_equal(as.matrix(eye(ddf)), diag(3))
  expect_equal(as.matrix(ones(ddf)), matrix(1, 3, 3))
  expect_equal(as.matrix(zeros(ddf)), matrix(0, 3, 3))
})
