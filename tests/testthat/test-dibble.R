test_that("dibble", {
  x1 <- tidyr::expand_grid(from = 1:10,
                          to = 1:11,
                          mode = 1:12) %>%
    dplyr::mutate(value = dplyr::row_number())
  x2 <- x1 %>%
    as_dibble(c("from", "to", "mode"))

  microbenchmark::microbenchmark(x1 = x1 %>%
                                   group_by(from, to) %>%
                                   summarise(value = sum(value),
                                             .groups = "drop"),
                                 x2 = x2 %>%
                                   group_by(from, to) %>%
                                   summarise(value = sum(value)))
})
