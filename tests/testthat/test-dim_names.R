test_that("dim_names", {
  x <- dibble(1:6,
              .dim_names = list(axis1 = letters[1:2],
                                axis2 = tibble::tibble(col1 = 1:3, col2 = 2:4)))

  y <- dibble(1:12,
              .dim_names = list(axis1 = letters[2:3],
                                axis2 = tibble::tibble(col1 = 2:4, col2 = 3:5),
                                axis3 = letters[3:4]))

  expect_equal(union_dim_names(list(dimnames(x), dimnames(y))),
               list(axis1 = letters[1:3],
                    axis2 = tibble::tibble(col1 = 1:4, col2 = 2:5),
                    axis3 = letters[3:4]))
  expect_equal(intersect_dim_names(list(dimnames(x), dimnames(y))),
               list(axis1 = letters[2],
                    axis2 = tibble::tibble(col1 = 2:3, col2 = 3:4)))
})
