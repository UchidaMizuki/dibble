test_that("dibble", {
  library(magrittr)

  x1 <- tidyr::expand_grid(from = letters[1:22],
                          to = letters[1:23],
                          mode = letters[1:24]) %>%
    dplyr::mutate(value = dplyr::row_number(),
                  value2 = value + 1,
                  value3 = "a")
  x2 <- x1 %>%
    dibble_by(from, to, mode)
  x3 <- x1 %>%
    cubelyr::as.tbl_cube(dim_names = c("from", "to", "mode"))

  microbenchmark::microbenchmark(x1 = x1 %>%
                                   group_by(from, to) %>%
                                   summarise(value = sum(value),
                                             .groups = "drop"),
                                 x2 = x2 %>%
                                   group_by(from, to) %>%
                                   summarise(value = sum(value)),
                                 x3 = x3 %>%
                                   group_by(from, to) %>%
                                   summarise(value = sum(value)))
})
