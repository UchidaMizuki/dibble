test_that("apply() works", {
  arr <- array(1:24, 2:4, list(axis1 = 1:2, axis2 = 1:3, axis3 = 1:4))
  expect_equal(apply(arr, 2:3, identity), base::apply(arr, 2:3, identity))

  test_apply <- function(x) {
    expect_true(all(apply(x, 2, sum) == apply(arr, 2, sum)))
    expect_true(all(apply(x, 2:3, sum) == apply(arr, 2:3, sum)))
    expect_true(all(apply(x, "axis2", sum) == apply(arr, "axis2", sum)))
    expect_true(all(
      apply(x, c("axis2", "axis3"), sum) == apply(arr, c("axis2", "axis3"), sum)
    ))
  }

  ddf_col <- dibble(arr)
  tbl_ddf <- dibble(value = arr)

  test_apply(ddf_col)
  test_apply(tbl_ddf)

  # Test that the class is preserved
  class(ddf_col) <- c("my_class", class(ddf_col))
  expect_s3_class(apply(ddf_col, 2, sum), c("my_class", "ddf_col"))

  class(tbl_ddf) <- c("my_class", class(tbl_ddf))
  expect_s3_class(apply(tbl_ddf, 2, sum), c("my_class", "ddf_col"))
})
