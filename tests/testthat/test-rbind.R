test_that("rbind.ddf_col() and rbind.tbl_ddf() work", {
  tbl_ddf1 <- dibble(x = 1:12,
                     y = 13:24,
                     .dim_names = list(axis1 = 1:4,
                                       axis2 = 1:3))
  tbl_ddf2 <- dibble(x = 1:12,
                     y = 13:24,
                     .dim_names = list(axis1 = 5:8,
                                       axis2 = 4:6))
  expect_error(rbind(tbl_ddf1, tbl_ddf1))

  tbl_ddf3 <- broadcast(rbind(tbl_ddf1, tbl_ddf2),
                        dim_names = c("axis1", "axis2"))
  expect_equal(dimnames(tbl_ddf3)[["axis1"]],
               vec_c(dimnames(tbl_ddf1)[["axis1"]],
                     dimnames(tbl_ddf2)[["axis1"]]))
  expect_equal(tbl_ddf3 |>
                 filter(axis1 %in% dimnames(tbl_ddf2)[["axis1"]],
                        axis2 %in% dimnames(tbl_ddf2)[["axis2"]]),
               tbl_ddf2)

  ddf_col1 <- tbl_ddf1[[1]]
  ddf_col2 <- tbl_ddf2[[1]]

  expect_error(rbind(ddf_col1, ddf_col1))

  ddf_col3 <- broadcast(rbind(ddf_col1, ddf_col2),
                        dim_names = c("axis1", "axis2"))
  expect_equal(dimnames(ddf_col3)[["axis1"]],
               vec_c(dimnames(ddf_col1)[["axis1"]],
                     dimnames(ddf_col2)[["axis1"]]))
  expect_equal(ddf_col3 |>
                 filter(axis1 %in% dimnames(ddf_col2)[["axis1"]],
                        axis2 %in% dimnames(ddf_col2)[["axis2"]]),
               ddf_col2)

  # works when the number of key rows is 1 (#23)
  data_1 <- tidyr::expand_grid(key = tibble::tibble(col_1 = 1,
                                                    col_2 = 1)) |>
    tibble::add_column(value = 1) |>
    dibble_by("key")

  data_2 <- tidyr::expand_grid(key = tibble::tibble(col_1 = 2:3,
                                                    col_2 = 2:3)) |>
    tibble::add_column(value = 2) |>
    dibble_by("key")

  data_3 <- tidyr::expand_grid(key = tibble::tibble(col_1 = 2,
                                                    col_2 = 2)) |>
    tibble::add_column(value = 2) |>
    dibble_by("key")

  data_4 <- tidyr::expand_grid(key = tibble::tibble(col_1 = 2,
                                                    col_2 = 2),
                               key_2 = tibble::tibble(col_1 = 2)) |>
    tibble::add_column(value = 2) |>
    dibble_by("key", "key_2")

  expect_no_error(broadcast(rbind(data_1, data_2), "key"))
  expect_no_error(broadcast(rbind(data_1, data_3), "key"))
  expect_no_error(broadcast(rbind(data_1, data_4), c("key", "key_2")))
})
