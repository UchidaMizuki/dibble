test_that("mutate.tbl_ddf() works", {
  tbl_ddf <- dibble(x = 1:12,
                    y = 13:24,
                    .dim_names = list(axis1 = 1:4,
                                      axis2 = 1:3)) %>%
    mutate(z = x + y)

  expect_equal(tbl_ddf$z, tbl_ddf$x + tbl_ddf$y)
})
