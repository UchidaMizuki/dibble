test_that("dibble", {
  arr1 <- array(1:6, 2:3,
                list(axis1 = letters[1:2],
                     axis2 = letters[1:3]))
  arr2 <- array(1:12, 3:4,
                list(axis2 = letters[1:3],
                     axis3 = letters[1:4]))

  expect_s3_class(dibble(arr1), "ddf_col")
  expect_s3_class(dibble(value = arr1), "tbl_ddf")

  tbl_ddf <- dibble(value1 = arr1,
                    value2 = arr2,
                    .dim_names = c("axis1", "axis2", "axis3"))

  expect_s3_class(tbl_ddf, "tbl_ddf")
})

test_that("dibble_by", {
  library(dplyr)
  library(tidyr)

  df <- expand_grid(axis1 = letters[1:3],
                    axis2 = letters[1:3]) %>%
    mutate(value = row_number())

  expect_equal(df,
               df %>%
                 dibble_by(axis1, axis2) %>%
                 as_tibble())
})
