test_that("dibble", {
  x <- tidyr::expand_grid(from = letters[1:10],
                          to = letters[1:10],
                          mode = letters[1:10]) %>%
    dplyr::mutate(value = dplyr::row_number(),
                  value2 = value + 1) %>%
    as_dibble(dim_names = c("from", "to", "mode"))
})
