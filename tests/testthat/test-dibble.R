test_that("dibble", {
  dibble(tidyr::table1,
         .dim_names = list(country = NULL, year = NULL))

  x <- array(1:6, 2:3,
             dimnames = list(from = letters[1:2], to = letters[1:3]))
  dibble(value = x)
})

test_that("as_dibble", {
  country <- c("Afghanistan", "Brazil", "China")
  year <- c(1999, 2000)

  expect_error({
    as_dibble(tidyr::table1,
              dim_names = list(country = country))
  })

  x <- as_dibble(tidyr::table1,
                 dim_names = list(country = country,
                                  year = year))
  x1 <- as_dibble(tidyr::table1,
                  dim_names = list(country = NULL,
                                   year = NULL))
  x2 <- dibble_by(tidyr::table1, country, year)
  x3 <- dibble_by(tidyr::table1, dplyr::all_of(c("country", "year")))

  expect_equal(x, x1)
  expect_equal(x, x2)
  expect_equal(x, x3)
})
