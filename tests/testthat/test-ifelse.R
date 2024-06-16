test_that("ifelse() works", {
  expect_equal(ifelse(1:3 == 2, 4, 5),
               base::ifelse(1:3 == 2, 4, 5))

  tbl_ddf <- dibble(x = 1:3,
                    .dim_names = list(axis = 1:3))
  ddf_col <- tbl_ddf$x
  expect_equal(as.vector(ifelse(tbl_ddf$x == 2, 4, 5)),
               base::ifelse(1:3 == 2, 4, 5))
  expect_equal(as.vector(ifelse(ddf_col == 2, 4, 5)),
               base::ifelse(1:3 == 2, 4, 5))
})
