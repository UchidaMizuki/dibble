test_that("dibble_pack() works", {
  set.seed(1234)

  n <- 5
  df <- tibble::tibble(axis2_value1 = sample(letters, n),
                       axis1_value2 = sample(letters, n),
                       axis1_value1 = sample(letters, n),
                       axis2_value2 = sample(letters, n),
                       value = rnorm(n))

  ddf <- df |>
    dibble_by(axis2_value1, axis1_value2, axis1_value1, axis2_value2,
              .names_sep = "_")

  ddf2 <- ddf |>
    dibble_pack(axis1 = c(axis1_value1, axis1_value2),
                axis2 = c(axis2_value1, axis2_value2),
                .names_sep = "_") |>
    dibble_unpack(c(axis1, axis2),
                  names_sep = "_") |>
    aperm(names(dimnames(ddf)))

  expect_equal(ddf, ddf2)
})
