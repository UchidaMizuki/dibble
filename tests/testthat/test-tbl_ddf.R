test_that("as.matrix.tbl_ddf() works", {
  x <- dibble(
    x = 1:6,
    y = 7:12,
    .dim_names = list(axis1 = letters[1:2], axis2 = letters[1:3])
  )
  expect_error(as.matrix(x))
  expect_equal(as.matrix(x[[1]]), matrix(1:6, 2, 3))
})

test_that("as_tibble.tbl_ddf() works", {
  dim_names <- list(axis1 = letters[1:2], axis2 = letters[1:3])
  x <- dibble(x = 1:6, y = 7:12, .dim_names = dim_names)
  expect_error(as.table(x))
  expect_s3_class(as.table(x[[1]]), "table")
  expect_equal(dimnames(as.table(x[[1]])), dim_names)
})

test_that("`dimnames<-.tbl_ddf`() works", {
  dim_names <- list(axis1 = letters[1:2], axis2 = letters[1:3])
  x <- dibble(x = 1:6, y = 7:12, .dim_names = list(axis1 = 1:2, axis2 = 1:3))
  dimnames(x) <- dim_names
  expect_equal(dimnames(x), dim_names)
})

test_that("as.data.frame.tbl_ddf() works", {
  x <- dibble(x = 1:6, y = 7:12, .dim_names = list(axis1 = 1:2, axis2 = 1:3))
  y <- expect_s3_class(as.data.frame(x), "data.frame")

  y <- y |>
    dibble_by("axis1", "axis2")
  expect_equal(y, x)
})

test_that("aperm.tbl_ddf() works", {
  x <- dibble(x = 1:6, y = 7:12, .dim_names = list(axis1 = 1:2, axis2 = 1:3))
  y <- aperm(x)
  expect_equal(y, t(x))

  y <- aperm(x, c(2, 1))
  expect_equal(y, t(x))

  x <- dibble(
    x = 1:24,
    .dim_names = list(axis1 = 1:2, axis2 = 1:3, axis3 = 1:4)
  )
  y <- array(1:24, c(2, 3, 4))
  expect_equal(as.array(aperm(x, c(3, 1, 2))), aperm(y, c(3, 1, 2)))

  # Test that the class is preserved
  class(x) <- c("my_class", class(x))
  y <- aperm(x, c(3, 1, 2))
  expect_s3_class(y, class(x))
})

test_that("`wrap_dibble() works", {
  set.seed(1234)

  # `!.tbl_ddf`()
  x <- sample(c(FALSE, TRUE), 6, replace = TRUE)
  tbl_ddf <- dibble(x = x, .dim_names = list(axis1 = 1:2, axis2 = 1:3))
  arr <- array(x, c(2, 3))

  expect_equal(!(!tbl_ddf), tbl_ddf[[1]])
  expect_equal(as.array(!tbl_ddf), !arr)

  # is.finite.tbl_ddf(), is.infinite.tbl_ddf(), is.nan.tbl_ddf(), is.na.tbl_ddf()
  x <- c(1, 2, NA, Inf, -Inf, NaN)
  tbl_ddf <- dibble(x = x, .dim_names = list(axis1 = 1:2, axis2 = 1:3))
  arr <- array(x, c(2, 3))

  expect_equal(as.array(is.finite(tbl_ddf)), is.finite(arr))
  expect_equal(as.array(is.infinite(tbl_ddf)), is.infinite(arr))
  expect_equal(as.array(is.nan(tbl_ddf)), is.nan(arr))
  expect_equal(as.array(is.na(tbl_ddf)), is.na(arr))

  # Test that the class is preserved
  class(tbl_ddf) <- c("my_class", class(tbl_ddf))
  expect_s3_class(!tbl_ddf, class(tbl_ddf))
})

test_that("`[.tbl_ddf`() works", {
  tbl_ddf <- dibble(
    x = 1:12,
    y = 13:24,
    .dim_names = list(axis1 = 1:4, axis2 = 1:3)
  )
  expect_equal(tbl_ddf[1], dibble(x = tbl_ddf[[1]]))
})

test_that("slice.tbl_ddf() works", {
  tbl_ddf <- dibble(
    x = 1:12,
    y = 13:24,
    .dim_names = list(axis1 = 1:4, axis2 = 1:3)
  )
  arr_x <- array(1:12, c(4, 3))
  arr_y <- array(13:24, c(4, 3))

  expect_equal(as.array(slice(tbl_ddf, 1:2, 2:3)$x), arr_x[1:2, 2:3])
  expect_equal(as.array(slice(tbl_ddf, 1:2, 2:3)$y), arr_y[1:2, 2:3])
})

test_that("mutate.tbl_ddf() works", {
  tbl_ddf <- dibble(
    x = 1:12,
    y = 13:24,
    .dim_names = list(axis1 = 1:4, axis2 = 1:3)
  ) |>
    mutate(z = x + y)

  expect_equal(tbl_ddf$z, tbl_ddf$x + tbl_ddf$y)
})

test_that("select.tbl_ddf() and relocate.tbl_ddf() work", {
  tbl_ddf <- dibble(
    x = 1:12,
    y = 13:24,
    .dim_names = list(axis1 = 1:4, axis2 = 1:3)
  )
  tbl_ddf_selected <- tbl_ddf |>
    select("axis2", "x")
  tbl_ddf_relocated <- tbl_ddf |>
    relocate("axis2")

  expect_equal(names(dimnames(tbl_ddf_selected)), c("axis2", "axis1"))
  expect_equal(tbl_ddf_selected, aperm(tbl_ddf, c("axis2", "axis1"))["x"])

  expect_equal(names(dimnames(tbl_ddf_relocated)), c("axis2", "axis1"))
  expect_equal(tbl_ddf_relocated, aperm(tbl_ddf, c("axis2", "axis1")))

  # Test that the class is preserved
  class(tbl_ddf) <- c("my_class", class(tbl_ddf))
  expect_s3_class(select(tbl_ddf, "axis2"), class(tbl_ddf))
})

test_that("rename.tbl_ddf() works", {
  tbl_ddf <- dibble(
    x = 1:12,
    y = 13:24,
    .dim_names = list(axis1 = 1:4, axis2 = 1:3)
  )
  tbl_ddf_renamed <- tbl_ddf |>
    rename("axis1_2" = "axis1", "axis2_2" = "axis2", "x_2" = "x")
  expect_equal(names(dimnames(tbl_ddf_renamed)), c("axis1_2", "axis2_2"))
  expect_equal(names(tbl_ddf_renamed), c("x_2", "y"))

  # Test that the class is preserved
  class(tbl_ddf) <- c("my_class", class(tbl_ddf))
  expect_s3_class(rename(tbl_ddf, "axis1_2" = "axis1"), class(tbl_ddf))
})

test_that("filter.tbl_ddf() works", {
  tbl_ddf <- dibble(
    x = 1:6,
    y = 7:12,
    .dim_names = list(axis1 = 1:2, axis2 = 1:3)
  )
  arr <- array(1:6, c(2, 3))

  tbl_ddf_filtered <- tbl_ddf |>
    filter(axis1 == 2)
  expect_equal(as.vector(tbl_ddf_filtered[["x"]]), arr[2, ])

  # .data
  tbl_ddf_filtered <- tbl_ddf |>
    filter(.data$axis1 == 2)
  expect_equal(as.vector(tbl_ddf_filtered[["x"]]), arr[2, ])

  name <- "axis1"
  tbl_ddf_filtered <- tbl_ddf |>
    filter(.data[[name]] == 2)
  expect_equal(as.vector(tbl_ddf_filtered[["x"]]), arr[2, ])

  # .env
  value <- 2
  tbl_ddf_filtered <- tbl_ddf |>
    filter(.data$axis1 == .env$value)
  expect_equal(as.vector(tbl_ddf_filtered[["x"]]), arr[2, ])

  name <- "value"
  tbl_ddf_filtered <- tbl_ddf |>
    filter(.data$axis1 == .env[[name]])
  expect_equal(as.vector(tbl_ddf_filtered[["x"]]), arr[2, ])

  # Test that the class is preserved
  class(tbl_ddf) <- c("my_class", class(tbl_ddf))
  expect_s3_class(filter(tbl_ddf, axis1 == 2), class(tbl_ddf))
})

test_that("print.tbl_ddf() works", {
  tbl_ddf <- dibble(
    x = 1:6,
    y = 7:12,
    .dim_names = list(axis1 = 1:2, axis2 = 1:3)
  )
  expect_output(print(tbl_ddf))
})
