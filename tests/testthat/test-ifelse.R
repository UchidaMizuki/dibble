test_that("ifelse", {
  expect_equal(ifelse(1:3 == 2, 4, 5),
               base::ifelse(1:3 == 2, 4, 5))

  ddf_col <- broadcast(1:3,
                       list(axis = 1:3))
  expect_equal(as.vector(ifelse(ddf_col == 2, 4, 5)),
               base::ifelse(1:3 == 2, 4, 5))
})
