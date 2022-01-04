test_that("dibble_metric", {
  x <- array(1:6, 2:3)
  dim_names <- list(axis1 = letters[1:2], axis2 = letters[1:3])

  x1 <- dibble_metric(x, dim_names)

  dimnames(x) <- dim_names

  x2 <- dibble_metric(x1)
  x3 <- dibble_metric(x2)

  expect_equal(as.array(x1), unname(x))
  expect_equal(as.array(x2), unname(x))
  expect_equal(as.array(x3), unname(x))
})

test_that("as_dibble_metric", {
  x <- as_dibble_metric(1,
                        dim_names = list(axis1 = 1:2, axis2 = 1:3))
  expect_equal(as.array(x), array(1, 2:3))

  x <- array(1:6, 2:3,
             dimnames = list(axis1 = letters[1:2], axis2 = letters[1:3]))
  expect_silent(as_dibble_metric(x))

  dim_names <- list(axis1 = NULL, axis2 = NULL, axis3 = letters[1:3])
  expect_warning(x1 <- as_dibble_metric(x, dim_names))

  x2 <- abind::abind(rep_len(list(unname(x)), 3),
                     rev.along = 0)
  dimnames(x2) <- NULL

  expect_equal(as.array(x1), x2)
})

test_that("is_dibble_metric", {
  expect_false(is_dibble_metric(table1_dibble))
  expect_true(is_dibble_metric(table1_dibble$cases))
})

test_that("as.table.dibble_metric", {
  x <- array(1:6, 2:3,
             dimnames = list(axis1 = letters[1:2], axis2 = letters[1:3]))
  x <- as.table(x)

  x1 <- as_dibble_metric(x)
  x1 <- as.table(x1)

  expect_equal(x, x1)
})

test_that("dimnames.dibble_metric", {
  x <- table1_dibble$cases
  dim_names <- dimnames(x)
  expect_equal(names(dim_names), c("country", "year"))

  dm <- dim(x)

  dim_names$country <- paste0(dim_names$country, "1")
  nms <- c("country1", "year")
  names(dim_names) <- nms

  dimnames(x) <- dim_names
  expect_equal(dimnames(x), dim_names)

  expect_equal(unname(dim(x)), unname(dm))
})

test_that("nrow-ncol-row-colnames.dibble_metric", {
  x <- table1_dibble$cases

  expect_equal(nrow(x), nrow(table1_dibble))
  expect_equal(ncol(x), NULL)

  expect_equal(rownames(x), NULL)
  expect_equal(colnames(x), NULL)
})

test_that("aperm.grouped_dibble", {
  x <- table1_dibble$cases
  x1 <- aperm(x)
  x2 <- aperm(x, c(2, 1))

  expect_equal(names(dim(x1)), c("year", "country"))
  expect_equal(x1, x2)
})

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

test_that("Ops.dibble_metric", {
  x <- array(1:6, 2:3,
             dimnames = list(axis1 = letters[1:2], axis2 = letters[1:3]))
  y <- array(1:6, 2:3,
             dimnames = list(axis3 = letters[1:2], axis4 = letters[1:3]))
  z <- x %o% y

  x1 <- as_dibble_metric(x)
  y1 <- as_dibble_metric(y)
  suppressWarnings({ z1 <- x1 * y1 })

  expect_equal(as.table(z1), as.table(z))
})

test_that("solve.dibble_metric", {
  set.seed(1234)
  x <- array(rnorm(1:9), c(3, 3),
             dimnames = list(axis1 = 1:3, axis2 = 1:3))

  x1 <- unname(solve(x))

  x2 <- as_dibble_metric(x)
  x2 <- solve(x2)
  x2 <- as.array(x2)

  expect_equal(x1, x2)
})

test_that("slice.dibble_metric", {
  x <- dplyr::filter(tidyr::table1,
                     country == "Afghanistan",
                     year == 1999)
  x <- dibble_by(x, country, year)

  x1 <- slice(table1_dibble$population, 1, 1)

  expect_equal(x$population, x1)
})

test_that("select.dibble_metric", {
  x <- table1_dibble$population
  x <- select(x, year)

  expect_equal(names(dimnames(x)), c("year", "country"))
})

test_that("relocate.dibble_metric", {
  x <- table1_dibble$population
  x <- relocate(x, year)

  expect_equal(names(dimnames(x)), c("year", "country"))
})

test_that("rename.dibble_metric", {
  x <- table1_dibble$population
  x <- rename(x,
              country1 = country,
              year1 = year)

  expect_equal(names(dimnames(x)), c("country1", "year1"))
})

test_that("print.dibble_metric", {
  expect_output(print(table1_dibble$cases))
})
