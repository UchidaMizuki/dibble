test_that("replace_na.ddf_col() and replace_na.tbl_ddf() work", {
  tbl_ddf <- dibble(x = 1:12,
                    y = 13:24,
                    .dim_names = list(axis1 = 1:4,
                                      axis2 = 1:3))
  tbl_ddf_na <- tbl_ddf |>
    mutate(x = ifelse(x %% 2 == 0, NA, x),
           y = ifelse(y %% 2 == 0, NA, y))

  expect_equal(replace_na(tbl_ddf_na, list(x = 0, y = 1)),
               tbl_ddf |>
                 mutate(x = ifelse(x %% 2 == 0, 0, x),
                        y = ifelse(y %% 2 == 0, 1, y)))

  ddf_col <- tbl_ddf[[1]]
  ddf_col_na <- ifelse(ddf_col %% 2 == 0, NA, ddf_col)

  expect_equal(replace_na(ddf_col_na, 2),
               ifelse(ddf_col %% 2 == 0, 2, ddf_col))
})
