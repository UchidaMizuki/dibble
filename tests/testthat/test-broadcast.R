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
