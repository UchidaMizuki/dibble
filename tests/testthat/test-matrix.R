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
