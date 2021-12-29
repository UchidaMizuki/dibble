test_that("dibble", {
  x1 <- tidyr::expand_grid(from = letters[1:24],
                          to = letters[1:25],
                          mode = letters[1:26],
                          mode2 = 1:1e1) %>%
    dplyr::mutate(value = dplyr::row_number(),
                  value2 = value + 1,
                  value3 = "a")
  x2 <- x1 %>%
    dibble_by(from, to, mode, mode2)
  # x3 <- x1 %>%
  #   cubelyr::as.tbl_cube(dim_names = c("from", "to", "mode"))

  microbenchmark::microbenchmark(x1 = x1 %>%
                                   group_by(from, to),
                                 x2 = x2 %>%
                                   group_by(from, to))
})
