test_that("as.matrix.ddf_col() works", {
  x <- broadcast(1:2, dim_names = list(axis1 = 1:2))
  expect_equal(as.matrix(x), matrix(1:2))

  x <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  expect_equal(as.matrix(x), matrix(1:6, 2, 3))

  x <- broadcast(1:24, dim_names = list(axis1 = 1:2, axis2 = 1:3, axis3 = 1:4))
  expect_error(as.matrix(x))
})

test_that("as.table.ddf_col() works", {
  dim_names <- list(axis1 = letters[1:2])
  x <- broadcast(1:2, dim_names = dim_names)
  expect_s3_class(as.table(x), "table")
  expect_equal(dimnames(as.table(x)), dim_names)

  dim_names <- list(axis1 = letters[1:2], axis2 = letters[1:3])
  x <- broadcast(1:6, dim_names = dim_names)
  expect_s3_class(as.table(x), "table")
  expect_equal(dimnames(as.table(x)), dim_names)
})

test_that("`dimnames<-.ddf_col`() works", {
  dim_names <- list(axis1 = letters[1:2], axis2 = letters[1:3])
  x <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  dimnames(x) <- dim_names
  expect_equal(dimnames(x), dim_names)
})

test_that("as.data.frame.ddf_col() works", {
  x <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  y <- expect_s3_class(as.data.frame(x), "data.frame")

  y <- y |>
    dibble_by("axis1", "axis2")
  y <- y[[1]]
  expect_equal(y, x)
})

test_that("aperm.ddf_col() works", {
  x <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  y <- aperm(x)
  expect_equal(y, t(x))

  y <- aperm(x, c(2, 1))
  expect_equal(y, t(x))

  x <- broadcast(1:24, dim_names = list(axis1 = 1:2, axis2 = 1:3, axis3 = 1:4))
  y <- array(1:24, c(2, 3, 4))
  expect_equal(as.array(aperm(x, c(3, 1, 2))), aperm(y, c(3, 1, 2)))

  # Test that the class is preserved
  class(x) <- c("my_class", class(x))
  y <- aperm(x, c(3, 1, 2))
  expect_s3_class(y, class(x))
})

test_that("`wrap_ddf_col() works", {
  set.seed(1234)

  # `!.ddf_col`()
  x <- sample(c(FALSE, TRUE), 6, replace = TRUE)
  ddf_col <- broadcast(x, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  arr <- array(x, c(2, 3))

  expect_equal(!(!ddf_col), ddf_col)
  expect_equal(as.array(!ddf_col), !arr)

  # is.finite.ddf_col(), is.infinite.ddf_col(), is.nan.ddf_col(), is.na.ddf_col()
  x <- c(1, 2, NA, Inf, -Inf, NaN)
  ddf_col <- broadcast(x, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  arr <- array(x, c(2, 3))

  expect_equal(as.array(is.finite(ddf_col)), is.finite(arr))
  expect_equal(as.array(is.infinite(ddf_col)), is.infinite(arr))
  expect_equal(as.array(is.nan(ddf_col)), is.nan(arr))
  expect_equal(as.array(is.na(ddf_col)), is.na(arr))

  # Test that the class is preserved
  class(ddf_col) <- c("my_class", class(ddf_col))
  expect_s3_class(!ddf_col, class(ddf_col))
})

test_that("slice.ddf_col() works", {
  ddf_col <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  arr <- array(1:6, c(2, 3))

  expect_equal(as.array(slice(ddf_col, 1:2, 2:3)), arr[1:2, 2:3])
  expect_equal(as.vector(slice(ddf_col, 1, 2:3)), arr[1, 2:3])

  # Test that the class is preserved
  class(ddf_col) <- c("my_class", class(ddf_col))
  expect_s3_class(slice(ddf_col, 1:2, 2:3), class(ddf_col))
})

test_that("select.ddf_col() and relocate.ddf_col() work", {
  ddf_col <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  ddf_col_selected <- ddf_col |>
    select("axis2")
  ddf_col_relocated <- ddf_col |>
    relocate("axis2")

  expect_equal(names(dimnames(ddf_col_selected)), c("axis2", "axis1"))
  expect_equal(ddf_col_selected, aperm(ddf_col, c("axis2", "axis1")))

  expect_equal(names(dimnames(ddf_col_relocated)), c("axis2", "axis1"))
  expect_equal(ddf_col_relocated, aperm(ddf_col, c("axis2", "axis1")))

  # Test that the class is preserved
  class(ddf_col) <- c("my_class", class(ddf_col))
  expect_s3_class(select(ddf_col, "axis2"), class(ddf_col))
})

test_that("rename.ddf_col() works", {
  ddf_col <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  ddf_col_renamed <- ddf_col |>
    rename("axis1_2" = "axis1", "axis2_2" = "axis2")
  expect_equal(names(dimnames(ddf_col_renamed)), c("axis1_2", "axis2_2"))

  # Test that the class is preserved
  class(ddf_col) <- c("my_class", class(ddf_col))
  expect_s3_class(rename(ddf_col, "axis1_2" = "axis1"), class(ddf_col))
})

test_that("filter.ddf_col() works", {
  ddf_col <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  arr <- array(1:6, c(2, 3))

  ddf_col_filtered <- ddf_col |>
    filter(axis1 == 2)
  expect_equal(as.vector(ddf_col_filtered), arr[2, ])

  # .data
  ddf_col_filtered <- ddf_col |>
    filter(.data$axis1 == 2)
  expect_equal(as.vector(ddf_col_filtered), arr[2, ])

  name <- "axis1"
  ddf_col_filtered <- ddf_col |>
    filter(.data[[name]] == 2)
  expect_equal(as.vector(ddf_col_filtered), arr[2, ])

  # .env
  value <- 2
  ddf_col_filtered <- ddf_col |>
    filter(.data$axis1 == .env$value)
  expect_equal(as.vector(ddf_col_filtered), arr[2, ])

  name <- "value"
  ddf_col_filtered <- ddf_col |>
    filter(.data$axis1 == .env[[name]])
  expect_equal(as.vector(ddf_col_filtered), arr[2, ])

  # Test that the class is preserved
  class(ddf_col) <- c("my_class", class(ddf_col))
  expect_s3_class(filter(ddf_col, axis1 == 2), class(ddf_col))
})

test_that("print.ddf_col() works", {
  ddf_col <- broadcast(1:6, dim_names = list(axis1 = 1:2, axis2 = 1:3))
  expect_output(print(ddf_col))
})
