test_that("broadcast", {
  x <- broadcast(1:2,
                 list(axis1 = letters[1:2]))
  expect_equal(as.array(x),
               array(1:2, 2))

  y <- broadcast(1:3,
                 list(axis2 = letters[1:3]))
  expect_equal(as.array(y),
               array(1:3, 3))

  expect_silent(broadcast(x * y, c("axis1", "axis2")))
})

test_that("broadcast-warn", {
  x <- broadcast(1:4,
                 list(axis1 = 1:2,
                      axis2 = 1:2))
  y <- x

  expect_silent(x * y)

  y <- t(x)

  expect_warning(x * y)
  expect_silent(broadcast(x * y,
                          c("axis1", "axis2")))

  y <- broadcast(1:6,
                 list(axis1 = 1:2,
                      axis2 = 1:3))
  expect_warning(x * y)
  expect_silent(broadcast(x * y,
                          c("axis1", "axis2")))

  y <- broadcast(1:3,
                 list(axis1 = 1:3))
  # expect_warning(x * y)
  expect_silent(broadcast(x * y,
                          c("axis1", "axis2")))
})
