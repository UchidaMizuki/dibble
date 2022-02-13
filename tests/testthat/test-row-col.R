test_that("nrow-ncol", {
  ddf_col <- broadcast(1:6,
                       list(axis1 = letters[1:2],
                            axis2 = letters[1:3]))
  expect_equal(nrow(ddf_col), 6)
  expect_equal(ncol(ddf_col), NULL)

  tbl_ddf <- dibble(value = ddf_col)
  expect_equal(nrow(tbl_ddf), 6)
  expect_equal(ncol(tbl_ddf), 1)
})

test_that("row-colnames", {
  ddf_col <- broadcast(1:6,
                       list(axis1 = letters[1:2],
                            axis2 = letters[1:3]))
  expect_equal(rownames(ddf_col), NULL)
  expect_equal(colnames(ddf_col), NULL)

  tbl_ddf <- dibble(value = ddf_col)
  expect_equal(rownames(tbl_ddf), NULL)
  expect_equal(colnames(tbl_ddf), "value")
})
