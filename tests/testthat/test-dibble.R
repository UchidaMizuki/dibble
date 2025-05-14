test_that("dibble() works", {
  arr1 <- array(1:6, 2:3, list(axis1 = letters[1:2], axis2 = letters[1:3]))
  arr2 <- array(1:12, 3:4, list(axis2 = letters[1:3], axis3 = letters[1:4]))

  expect_s3_class(dibble(arr1), "ddf_col")
  expect_s3_class(dibble(value = arr1), "tbl_ddf")

  tbl_ddf <- dibble(
    value1 = arr1,
    value2 = arr2,
    .dim_names = c("axis1", "axis2", "axis3")
  )

  expect_s3_class(tbl_ddf, "tbl_ddf")

  tbl_ddf2 <- dibble(tbl_ddf)
  expect_equal(tbl_ddf2, tbl_ddf)

  expect_error(dibble(value = tbl_ddf))
  expect_no_error(dibble(value = tbl_ddf[[1]]))
})

test_that("dibble_by() works", {
  df <- tidyr::expand_grid(axis1 = letters[1:3], axis2 = letters[1:3]) |>
    dplyr::mutate(value = dplyr::row_number())

  expect_equal(
    df,
    df |>
      dibble_by(axis1, axis2) |>
      as_tibble()
  )
})

test_that("as_dibble() and is_dibble() work", {
  df <- tidyr::expand_grid(axis1 = letters[1:3], axis2 = letters[1:3]) |>
    dplyr::mutate(value = dplyr::row_number())

  tbl_ddf <- expect_equal(
    df |>
      dplyr::group_by(axis1, axis2) |>
      as_dibble(),
    df |>
      dibble_by(axis1, axis2)
  )

  expect_equal(as_dibble(tbl_ddf), tbl_ddf)
  expect_equal(as_dibble(tbl_ddf[[1]]), tbl_ddf[[1]])

  expect_false(is_dibble(df))
  expect_true(is_dibble(tbl_ddf))
  expect_true(is_dibble(tbl_ddf[[1]]))
})
