test_that("group_by.grouped_dibble", {
  # x <- group_by(table1_dibble,
  #               year)
  #
  # expect_true(is_grouped_dibble(x))
  #
  # x <- ungroup(x)
  # x <- relocate(x, country)
  #
  # expect_equal(x, table1_dibble)
})

test_that("ungroup", {
  # x <- group_by(table1_dibble,
  #               country)
  #
  # expect_equal(ungroup(x), table1_dibble)
  # expect_equal(ungroup(table1_dibble), table1_dibble)
  # expect_equal(ungroup(table1_dibble$cases), table1_dibble$cases)
})

test_that("dimnames.grouped_dibble", {
  # dim_names <- dimnames(table1_dibble)
  # x <- group_by(table1_dibble,
  #               year)
  # dim_names1 <- dimnames(x)
  # expect_equal(dim_names, dim_names1[names(dim_names)])
  #
  # dim_names$year <- paste0(dim_names$year, "1")
  # names(dim_names)[[2]] <- "year1"
  #
  # expect_error(dimnames(x) <- dim_names)
  # dim_names <- dim_names[c("year1", "country")]
  # dimnames(x) <- dim_names
  #
  # expect_equal(dimnames(x), dim_names)
  # expect_equal(dim(x), lengths(dim_names))
})

test_that("nrow-ncol-row-colnames.grouped_dibble", {
  # x <- group_by(table1_dibble,
  #               year)
  #
  # expect_equal(nrow(x), nrow(table1_dibble))
  # expect_equal(ncol(x), 2)
  #
  # expect_equal(rownames(x), NULL)
  # expect_equal(colnames(x), c("cases", "population"))
})

test_that("as_tibble.grouped_dibble", {
  # x <- as_tibble(table1_dibble)
  # y <- group_by(table1_dibble,
  #               year)
  # y <- as_tibble(y)
  # y <- dplyr::arrange(y, country, year)
  #
  # expect_equal(x, y[names(x)])
})

test_that("subsetting-dibble", {
  # x <- group_by(table1_dibble,
  #               year)
  #
  # expect_true(is_grouped_dibble(x["population"]))
  # expect_true(is_grouped_dibble(x[1]))
  # expect_true(is_dibble_measure(x[["population"]]))
  # expect_true(is_dibble_measure(x[[1L]]))
  #
  # expect_equal(table1_dibble[["population"]], relocate(x[["population"]], country))
  # expect_equal(table1_dibble$population, relocate(x$population, country))
})

test_that("slice.grouped_dibble", {
  # x <- dplyr::filter(tidyr::table1,
  #                    country == "Afghanistan",
  #                    year == 1999)
  # x <- dibble_by(x, country, year)
  # x <- group_by(x, year)
  #
  # x1 <- group_by(table1_dibble, year)
  # x1 <- slice(x1, 1, 1)
  #
  # expect_equal(x, x1)
})

test_that("mutate.grouped_dibble", {
  # x <- group_by(table1_dibble,
  #               year)
  # x <- mutate(x,
  #             population = population + 1,
  #             cases_population = cases * population)
  # x1 <- mutate(table1_dibble,
  #              population = population + 1,
  #              cases_population = cases * population)
  # expect_equal(relocate(ungroup(x), country), x1)
})

test_that("summarise.grouped_dibble", {
  # x <- group_by(table1_dibble,
  #               country)
  # x <- summarise(x,
  #                population = mean(population))
  #
  # x1 <- group_by(tidyr::table1,
  #                country)
  # x1 <- summarise(x1,
  #                 population = mean(population))
  # expect_equal(as_tibble(x), x1)
})

test_that("select.grouped_dibble", {
  # x <- group_by(table1_dibble,
  #               year)
  # x <- select(x,
  #             cases)
  # x <- ungroup(x)
  # x <- relocate(x,
  #               country, year)
  #
  # x1 <- select(table1_dibble,
  #              cases)
  #
  # expect_equal(colnames(x), "cases")
  # expect_equal(x, x1)
})

test_that("relocate.grouped_dibble", {
  # x <- group_by(table1_dibble,
  #               country)
  # x <- relocate(x,
  #               year, country, population, cases)
  # expect_equal(names(dimnames(x)), c("country", "year"))
  # expect_equal(colnames(x), c("population", "cases"))
})

test_that("rename.grouoped_dibble", {
  # x <- group_by(table1_dibble, year)
  # x <- rename(x,
  #             year1 = year,
  #             country1 = country,
  #             population1 = population,
  #             cases1 = cases)
  # expect_true(setequal(names(dimnames(x)), c("year1", "country1")))
  # expect_true(setequal(colnames(x), c("population1", "cases1")))
})

test_that("print.grouped_dibble", {
  # x <- group_by(table1_dibble, year)
  # expect_output(print(x))
})
