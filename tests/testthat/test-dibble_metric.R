test_that("as_tibble.dibble_metric", {
  x <- table1_dibble$population
  expect_equal(dimnames(x), dimnames(table1_dibble))
})

test_that("aperm.dibble_metric", {
  x <- aperm(table1_dibble$population)
  x1 <- aperm(table1_dibble$population, c(2, 1))

  expect_equal(names(dim(x)), c("year", "country"))
  expect_equal(x, x1)

  expect_equal(as.array(x), aperm(as.array(table1_dibble$population)))
})

test_that("slice.dibble_metric", {
  x <- dplyr::filter(tidyr::table1,
                     country == "Afghanistan",
                     year == 1999)
  x <- dibble_by(x, country, year)

  x1 <- slice(table1_dibble$population, 1, 1)

  expect_equal(x$population, x1)
})
