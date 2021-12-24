test_that("dibble", {
  x <- tidyr::expand_grid(from = letters[1:10],
                          to = letters[1:10],
                          mode = letters[1:10]) %>%
    dplyr::mutate(value = dplyr::row_number(),
                  value2 = value + 1) %>%
    as_dibble(dim_names = c("from", "to", "mode"))

  x2 <- tidyr::expand_grid(from = letters[1:10],
                           to = letters[1:10]) %>%
    dplyr::mutate(value = rnorm(dplyr::n())) %>%
    as_dibble(dim_names = c("from", "to"))

  x3 <- list(a = array(1:4, c(2, 2)),
             b = array(1:4, c(2, 2)))
  x4 <- abind::abind(x3, along = 3)
  apply(x4, 3,
        function(x) x,
        simplify = FALSE)
})
