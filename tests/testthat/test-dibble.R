test_that("dibble", {
  x <- array(1:6, 2:3,
             dimnames = list(from = letters[1:2], to = letters[1:3]))
  expect_equal(x, as.array(dibble(value = x)$value))

  expect_equal(population_dibble, dibble(population_dibble))
  expect_equal(population_dibble, dibble(population = population_dibble$population))

  expect_equal(dibble(tidyr::table1, .dim_names = list(country = NULL, year = NULL)), table1_dibble)
})

test_that("as_dibble", {
  country <- c("Afghanistan", "Brazil", "China")
  year <- c(1999, 2000)
  expect_error(as_dibble(tidyr::table1,
                         dim_names = list(country = country)))

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

test_that("is_dibble", {
  expect_true(is_dibble(population_dibble))
})

test_that("as.list.dibble", {
  x <- as.list(population_dibble)
  expect_true(all(purrr::map_lgl(x, is_dibble_metric)))
})

test_that("dimnames.dibble", {
  dim_names <- dimnames(population_dibble)
  expect_equal(names(dim_names), c("country", "year"))

  x <- population_dibble
  dm <- dim(x)

  dim_names$country <- paste0(dim_names$country, "1")
  nms <- c("country1", "year")
  names(dim_names) <- nms

  dimnames(x) <- dim_names
  expect_equal(dimnames(x), dim_names)

  expect_equal(unname(dim(x)), unname(dm))
})

test_that("nrow-ncol-row-colnames.default", {
  expect_equal(nrow(tidyr::table1), base::nrow(tidyr::table1))
  expect_equal(ncol(tidyr::table1), base::ncol(tidyr::table1))
  expect_equal(rownames(tidyr::table1), base::rownames(tidyr::table1))
  expect_equal(colnames(tidyr::table1), base::colnames(tidyr::table1))
})

test_that("nrow-ncol-row-colnames.dibble", {
  x <- tidyr::expand(tidyr::population, country, year)

  expect_equal(nrow(population_dibble), nrow(x))
  expect_equal(ncol(population_dibble), 1)

  expect_equal(rownames(population_dibble), NULL)
  expect_equal(colnames(population_dibble), "population")
})

test_that("as_tibble.dibble", {
  x <- tidyr::complete(tidyr::table1,
                       country, year)

  y <- as_tibble(table1_dibble)
  y <- dplyr::rename(y, population1 = population)

  x <- dplyr::left_join(x, y,
                        by = c("country", "year"))

  expect_equal(x$population, x$population1)
})

test_that("aperm.dibble", {
  x <- aperm(table1_dibble)
  x1 <- aperm(table1_dibble, c(2, 1))

  expect_equal(names(dim(x)), c("year", "country"))
  expect_equal(x, x1)
})

test_that("subsetting-dibble", {
  expect_equal(population_dibble["population"], population_dibble)

  x <- table1_dibble
  x["cases1"] <- x["cases"]

  expect_equal(x$cases, x$cases1)
  expect_equal(table1_dibble$population, table1_dibble[["population"]])

  x[["population1"]] <- x[["population"]]
  expect_equal(x[["population1"]], x[["population"]])
  expect_equal(x$population1, x$population)

  x$population2 <- x$population
  expect_equal(x$population2, x$population)
})

test_that("slice.dibble", {
  x <- dplyr::filter(tidyr::table1,
                     country == "Afghanistan",
                     year == 1999)
  x <- dibble_by(x, country, year)

  x1 <- slice(table1_dibble, 1, 1)

  expect_equal(x, x1)
})

test_that("mutate.dibble", {
  x <- mutate(table1_dibble,
              population = population + 1,
              cases_population = cases * population)

  expect_equal(as.array(x$population), as.array(table1_dibble$population) + 1)
  expect_equal(as.array(x$cases_population), as.array(table1_dibble$cases) * (as.array(table1_dibble$population) + 1))
})

# test_that("select.dibble", {
#
# })
