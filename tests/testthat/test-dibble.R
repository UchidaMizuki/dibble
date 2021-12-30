test_that("dibble", {
  library(magrittr)

  x1 <- tidyr::expand_grid(from = letters[1:22],
                          to = letters[1:23],
                          mode = letters[1:24],
                          mode2 = 1:1e2) %>%
    dplyr::mutate(value = dplyr::row_number(),
                  value2 = value + 1,
                  value3 = "a")
  x2 <- x1 %>%
    dibble_by(from, to, mode, mode2)
  x3 <- x1 %>%
    cubelyr::as.tbl_cube(dim_names = c("from", "to", "mode", "mode2"))

  microbenchmark::microbenchmark(x1 = x1 %>%
                                   group_by(from, to, mode) %>%
                                   summarise(value = sum(value),
                                             .groups = "drop"),
                                 x2 = x2 %>%
                                   group_by(from, to, mode) %>%
                                   summarise(value = sum(value)),
                                 x3 = x3 %>%
                                   group_by(from, to, mode) %>%
                                   summarise(value = sum(value)))


  f <- function(x) {
    purrr::modify(x,
                  function(x) {
                    dm <- dim(x)
                    for (i in seq_along(x)) {
                      x[[i]] <- as.list(x[[i]])
                    }
                    array(x, dm)
                  })
  }

  f(list(array(1:6, 2:3)))
})
