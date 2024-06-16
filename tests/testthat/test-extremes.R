test_that("pmax() and pmin() work", {
  x <- c(1, 2, 3, NA, 5, 6)
  y <- c(3, 2, 1, 5, NA, 6)

  x_tbl_ddf <- dibble(x = x,
                      .dim_names = list(axis1 = 1:2,
                                        axis2 = 1:3))
  y_tbl_ddf <- dibble(y = y,
                      .dim_names = list(axis1 = 1:2,
                                        axis2 = 1:3))
  x_ddf_col <- x_tbl_ddf$x
  y_ddf_col <- y_tbl_ddf$y

  x_arr <- array(x, dim = c(2, 3))
  y_arr <- array(y, dim = c(2, 3))

  expect_equal(as.array(pmax(x_tbl_ddf, y_tbl_ddf)), pmax(x_arr, y_arr))
  expect_equal(as.array(pmax(x_ddf_col, y_ddf_col)), pmax(x_arr, y_arr))
  expect_equal(as.array(pmax(x_ddf_col, y_ddf_col, na.rm = TRUE)),
               pmax(x_arr, y_arr, na.rm = TRUE))

  expect_equal(as.array(pmin(x_tbl_ddf, y_tbl_ddf)), pmin(x_arr, y_arr))
  expect_equal(as.array(pmin(x_ddf_col, y_ddf_col)), pmin(x_arr, y_arr))
  expect_equal(as.array(pmin(x_ddf_col, y_ddf_col, na.rm = TRUE)),
               pmin(x_arr, y_arr, na.rm = TRUE))

  # Test that the class is preserved
  class(x_tbl_ddf) <- c("my_class", class(x_tbl_ddf))
  expect_s3_class(pmax(x_tbl_ddf, y_tbl_ddf), c("my_class", "tbl_ddf"))
  expect_s3_class(pmin(x_tbl_ddf, y_tbl_ddf), c("my_class", "ddf_col"))

  class(x_ddf_col) <- c("my_class", class(x_ddf_col))
  expect_s3_class(pmax(x_ddf_col, y_ddf_col), c("my_class", "ddf_col"))
  expect_s3_class(pmin(x_ddf_col, y_ddf_col), c("my_class", "ddf_col"))
})
