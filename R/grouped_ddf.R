new_grouped_ddf <- function(x, dim_names) {
  structure(x,
            dim_names = dim_names,
            class = "grouped_ddf")
}

is_grouped_ddf <- function(x) {
  inherits(x, "grouped_ddf")
}

#' @importFrom dplyr group_keys
#' @export
group_keys.grouped_ddf <- function(.tbl, ...) {
  attr(.tbl, "dim_names")
}

#' @importFrom dplyr group_vars
#' @export
group_vars.grouped_ddf <- function(x) {
  names(group_keys(x))
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_ddf <- function(.data, ...) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)
  dim <- list_sizes(dim_names)
  size <- prod(dim)

  loc <- tidyselect::eval_select(expr(c(...)), dim_names)

  stopifnot(
    length(loc) < length(dim)
  )

  group_dim_names <- dim_names[loc]
  group_dim <- list_sizes(group_dim_names)

  dim_names <- dim_names[-loc]
  dim <- dim[-loc]

  .data <- lapply(undibble(.data),
                  function(x) {
                    x <- apply(x, loc,
                               function(x) {
                                 new_ddf_col(x, dim_names)
                               },
                               simplify = FALSE)
                    array(x, group_dim)
                  })
  new_grouped_ddf(.data, group_dim_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_ddf <- function(x, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- list_sizes(dim_names)

  group_axes <- group_vars(x)

  loc <- vec_match(group_axes, axes)
  dim <- c(dim[-loc], dim[loc])

  axes <- seq_along(axes)
  perm <- c(setdiff(axes, loc), loc)
  perm <- vec_match(axes, perm)

  x <- undibble(x)
  col_names <- names(x)
  x <- lapply(x,
              function(x) {
                x <- array(exec(c, !!!x),
                           dim = dim)
                aperm(x, perm)
              })
  names(x) <- col_names
  new_tbl_ddf(x, dim_names)
}

#' @export
as.array.grouped_ddf <- function(x, ...) {
  as.array(as_ddf_col(x), ...)
}

#' @export
as.table.grouped_ddf <- function(x, ...) {
  as.table(as_ddf_col(x), ...)
}

#' @export
dimnames.grouped_ddf <- function(x) {
  group_dim_names <- group_keys(x)

  x <- undibble(x)
  dim_names <- dimnames(x[[1L]][[1L]])

  c(group_dim_names, dim_names)
}

#' @export
`dimnames<-.grouped_ddf` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.grouped_ddf <- function(x) {
  dim_dibble(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.grouped_ddf <- function(x, ...) {
  as_tibble(ungroup(x), ...)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.grouped_ddf` <- function(x, i) {
  new_grouped_ddf(NextMethod(), group_keys(x))
}

#' @export
`[[.grouped_ddf` <- function(x, i) {
  x <- ungroup(x)
  x[[i]]
}

#' @export
`$.grouped_ddf` <- function(x, i) {
  x <- ungroup(x)
  x[[i]]
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.grouped_ddf <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom  dplyr mutate
#' @export
mutate.grouped_ddf <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)
  seq_nms <- seq_along(nms)

  group_dim_names <- group_keys(.data)
  group_dim <- list_sizes(group_dim_names)
  size <- prod(group_dim)

  .data <- undibble(.data)
  dim_names <- dimnames(.data[[1L]][[1L]])

  out <- .data
  out[setdiff(nms, names(out))] <- list(array(list(), group_dim))

  for (i in seq_len(size)) {
    data <- lapply(.data,
                   function(x) {
                     x[[i]]
                   })

    for (j in seq_nms) {
      nm <- nms[[j]]
      out[[nm]][[i]] <- data[[nm]] <- suppress_warning_broadcast(
        broadcast(eval_tidy(dots[[j]], data), dim_names)
      )
    }
  }
  new_grouped_ddf(out, group_dim_names)
}

#' @importFrom dplyr summarise
#' @export
summarise.grouped_ddf <- function(.data, ...) {
  dim_names <- group_keys(.data)
  dim <- list_sizes(dim_names)
  size <- prod(dim)

  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)
  seq_nms <- seq_along(nms)

  out <- array(dim = dim)
  out <- rep_len(list(out), vec_unique_count(nms))
  names(out) <- vec_unique(nms)

  .data <- undibble(.data)

  for (i in seq_len(size)) {
    data <- lapply(.data,
                   function(x) {
                     x[[i]]
                   })

    for (j in seq_nms) {
      nm <- nms[[j]]
      out[[nm]][[i]] <- data[[nm]] <- suppress_warning_broadcast(
        eval_tidy(dots[[j]], data)
      )
    }
  }
  new_tbl_ddf(out, dim_names)
}

#' @importFrom dplyr select
#' @export
select.grouped_ddf <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.grouped_ddf <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.grouped_ddf <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.grouped_ddf <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
