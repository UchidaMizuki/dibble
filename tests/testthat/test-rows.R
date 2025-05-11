test_that("rows", {
  check_rows <- function(x, y, axes) {
    # check insert
    expect_error(
      x |>
        dplyr::rows_insert(y)
    )

    check_insert <- function(x) {
      x_insert1 <- x |>
        dplyr::rows_insert(y, conflict = "ignore") |>
        broadcast(axes) |>
        tibble::as_tibble(n = "x")

      x_insert2 <- x |>
        tibble::as_tibble(n = "x") |>
        dplyr::rows_insert(
          y |>
            tibble::as_tibble(n = "x"),
          by = axes,
          conflict = "ignore"
        )

      x_insert <- x_insert1 |>
        dplyr::rename(x1 = x) |>
        dplyr::left_join(
          x_insert2 |>
            dplyr::rename(x2 = x),
          by = axes
        )

      expect_equal(x_insert$x1, x_insert$x2)
    }
    check_insert(x)
    check_insert(x$x)

    # check update
    expect_error(
      x |>
        dplyr::rows_update(y)
    )

    check_update <- function(x) {
      x_update1 <- x |>
        dplyr::rows_update(y, unmatched = "ignore") |>
        broadcast(axes) |>
        tibble::as_tibble(n = "x")

      x_update2 <- x |>
        tibble::as_tibble(n = "x") |>
        dplyr::rows_update(
          y |>
            tibble::as_tibble(n = "x"),
          by = axes,
          unmatched = "ignore"
        )

      x_update <- x_update1 |>
        dplyr::rename(x1 = x) |>
        dplyr::left_join(
          x_update2 |>
            dplyr::rename(x2 = x),
          by = axes
        )

      expect_equal(x_update$x1, x_update$x2)
    }
    check_update(x)
    check_update(x$x)

    # check patch
    expect_error(
      x |>
        dplyr::rows_patch(y)
    )

    check_patch <- function(x) {
      x_patch1 <- x |>
        dplyr::rows_patch(y, unmatched = "ignore") |>
        broadcast(axes) |>
        tibble::as_tibble(n = "x")

      x_patch2 <- x |>
        tibble::as_tibble(n = "x") |>
        dplyr::rows_patch(
          y |>
            tibble::as_tibble(n = "x"),
          by = axes,
          unmatched = "ignore"
        )

      x_patch <- x_patch1 |>
        dplyr::rename(x1 = x) |>
        dplyr::left_join(
          x_patch2 |>
            dplyr::rename(x2 = x),
          by = axes
        )

      expect_equal(x_patch$x1, x_patch$x2)
    }
    check_patch(x)
    check_patch(x$x)

    # check upsert
    expect_silent(
      x |>
        dplyr::rows_upsert(y) |>
        broadcast(axes)
    )

    check_upsert <- function(x) {
      x_upsert1 <- x |>
        dplyr::rows_upsert(y) |>
        broadcast(axes) |>
        tibble::as_tibble(n = "x")

      x_upsert2 <- x |>
        tibble::as_tibble(n = "x") |>
        dplyr::rows_upsert(
          y |>
            tibble::as_tibble(n = "x"),
          by = axes
        )

      x_upsert <- x_upsert1 |>
        dplyr::rename(x1 = x) |>
        dplyr::left_join(
          x_upsert2 |>
            dplyr::rename(x2 = x),
          by = axes
        )

      expect_equal(x_upsert$x1, x_upsert$x2)
    }
    check_upsert(x)
    check_upsert(x$x)
  }

  x <- dibble(
    x = c(NA, 1:4, NA),
    .dim_names = list(axis1 = rev(letters[1:2]), axis2 = rev(letters[1:3]))
  )

  y <- dibble(
    x = 1:6,
    .dim_names = list(axis1 = letters[2:3], axis2 = letters[2:4])
  )

  check_rows(x, y, c("axis1", "axis2"))

  x <- dibble(
    x = vec_rep(c(1:5, NA), 4),
    .dim_names = list(
      axis1 = vec_slice(tibble::tibble(col1 = 1:2, col2 = 2:3), rev(1:2)),
      axis2 = tibble::tibble(col1 = 1:3, col2 = 2:4),
      axis3 = vec_slice(tibble::tibble(col1 = 1:4, col2 = 2:5), rev(1:4))
    )
  )

  y <- dibble(
    x = vec_rep_each(c(NA, 1:7), 3),
    .dim_names = list(
      axis1 = tibble::tibble(col1 = 2:3, col2 = 3:4),
      axis2 = vec_slice(tibble::tibble(col1 = 1:3, col2 = 2:4), rev(1:3)),
      axis3 = tibble::tibble(col1 = 2:5, col2 = 3:6)
    )
  )

  check_rows(x, y, c("axis1", "axis2", "axis3"))
})
