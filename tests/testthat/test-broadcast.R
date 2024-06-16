test_that("broadcast() works", {
  x <- broadcast(1:2,
                 list(axis1 = letters[1:2]))
  expect_equal(as.array(x),
               array(1:2, 2))

  y <- broadcast(1:3,
                 list(axis2 = letters[1:3]))
  expect_equal(as.array(y),
               array(1:3, 3))

  z <- broadcast(1:4,
                 list(axis3 = letters[1:4]))
  expect_equal(as.array(z),
               array(1:4, 4))

  xy <- expect_silent(broadcast(x * y, c("axis1", "axis2")))
  expect_equal(as.array(xy), outer(as.array(x), as.array(y)))

  xyz <- expect_silent(broadcast(x * y * z, c("axis1", "axis2", "axis3")))
  expect_equal(as.array(xyz), outer(outer(as.array(x), as.array(y)), as.array(z)))

  # Test that the class is preserved
  class(x) <- c("my_class", class(x))
  xy <- broadcast(x * y, c("axis1", "axis2"))
  expect_s3_class(xy, class(x))
})

test_that("broadcast() warns", {
  x <- broadcast(1:4,
                 list(axis1 = 1:2,
                      axis2 = 1:2))
  y <- x

  xy1 <- expect_silent(x * y)

  y <- t(x)

  expect_warning(x * y)
  xy2 <- expect_silent(broadcast(x * y,
                                 c("axis1", "axis2")))
  expect_equal(xy1, xy2)

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
