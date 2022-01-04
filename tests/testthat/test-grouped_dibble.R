test_that("dimnames.grouped_dibble", {
  dim_names <- dimnames(table1_dibble)
  x <- group_by(table1_dibble,
                year)
  dim_names1 <- dimnames(x)
  expect_equal(dim_names, dim_names1[names(dim_names)])
})

test_that("as_tibble.grouped_dibble", {
  x <- as_tibble(table1_dibble)
  y <- group_by(table1_dibble,
                year)
  y <- as_tibble(y)
  y <- dplyr::arrange(y, country, year)

  expect_equal(x, y[names(x)])
})

test_that("slice.grouped_dibble", {
  x <- dplyr::filter(tidyr::table1,
                     country == "Afghanistan",
                     year == 1999)
  x <- dibble_by(x, country, year)
  x <- group_by(x, year)

  x1 <- group_by(table1_dibble, year)
  x1 <- slice(x1, 1, 1)

  expect_equal(x, x1)
})
