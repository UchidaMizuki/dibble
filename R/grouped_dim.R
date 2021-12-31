new_grouped_dim <- function(x, group_names) {
  class(x) <- "grouped_dim"
  attr(x, "group_names") <- group_names
  x
}

group_names <- function(x) {
  attr(x, "group_names")
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  dim_names <- dimnames(.data)
  axes <- names(dim_names)
  dim <- lengths(dim_names)
  size <- prod(dim)

  loc <- tidyselect::eval_select(expr(c(...)), dim_names)

  stopifnot(
    length(loc) < length(dim)
  )

  group_names <- dim_names[loc]
  group_dim <- lengths(group_names)

  dim_names <- dim_names[-loc]
  dim <- dim[-loc]

  .data <- purrr::modify(as_list_dibble(.data),
                         function(x) {
                           x <- apply(x, loc,
                                      function(x) {
                                        new_dim_col(x, dim_names)
                                      },
                                      simplify = FALSE)
                           array(x, group_dim)
                         })
  new_grouped_dim(.data,
                  group_names = group_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- lengths(dim_names)

  group_names <- group_names(x)
  group_axes <- names(group_names)

  loc <- vec_match(group_axes, axes)
  dim <- c(dim[-loc], dim[loc])

  axes <- seq_along(axes)
  perm <- c(setdiff(axes, loc), loc)
  perm <- vec_match(axes, perm)

  x <- as_list_dibble(x)
  col_names <- names(x)
  x <- purrr::modify(x,
                     function(x) {
                       x <- array(exec(c, !!!x),
                                  dim = dim)
                       aperm(x, perm)
                     })
  names(x) <- col_names
  new_dibble(x, dim_names)
}

is_grouped_dim <- function(x) {
  inherits(x, "grouped_dim")
}

#' @export
as.array.grouped_dim <- function(x, ...) {
  structure(x,
            class = NULL,
            group_names = NULL)
}

#' @export
dimnames.grouped_dim <- function(x) {
  group_names <- group_names(x)

  x <- as_list_dibble(x)
  dim_names <- dimnames(x[[1]][[1]])

  c(group_names, dim_names)
}

#' @export
dim.grouped_dim <- function(x) {
  lengths(dimnames(x))
}

#' @export
nrow.grouped_dim <- function(x) {
  nrow_dibble(x)
}

#' @export
ncol.grouped_dim <- function(x) {
  length(colnames(x))
}

#' @export
rownames.grouped_dim <- function(x, ...) {
  NULL
}

#' @export
colnames.grouped_dim <- function(x, ...) {
  names(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.grouped_dim <- function(x, ...) {
  as_tibble_dibble(x, ...)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.grouped_dim` <- function(x, i) {
  new_grouped_dim(NextMethod(), group_names(x))
}

#' @export
`[[.grouped_dim` <- function(x, i) {
  x <- ungroup(x)
  x[[i]]
}

#' @export
`$.grouped_dim` <- function(x, i) {
  x <- ungroup(x)
  x[[i]]
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.grouped_dim <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom  dplyr mutate
#' @export
mutate.grouped_dim <- function(.data, ...) {
  dots <- rlang::enquos(..., .named = TRUE)
  nms <- names(dots)
  seq_nms <- seq_along(nms)

  group_names <- group_names(.data)
  group_dim <- lengths(group_names)
  size <- prod(group_dim)

  .data <- as_list_dibble(.data)
  dim_names <- dimnames(.data[[1]][[1]])

  out <- .data
  out[setdiff(nms, names(out))] <- list(array(list(), group_dim))

  for (i in seq_len(size)) {
    data <- purrr::map(.data,
                       function(x) {
                         x[[i]]
                       })

    for (j in seq_nms) {
      nm <- nms[[j]]
      out[[nm]][[i]] <- data[[nm]] <- as_dim_col(eval_tidy(dots[[j]], data), dim_names)
    }
  }
  new_grouped_dim(out, group_names)
}

#' @importFrom dplyr summarise
#' @export
summarise.grouped_dim <- function(.data, ...) {
  dim_names <- group_names(.data)
  dim <- lengths(dim_names)
  size <- prod(dim)

  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)
  seq_nms <- seq_along(nms)

  out <- array(dim = dim)
  out <- rep_len(list(out), vec_unique_count(nms))
  names(out) <- unique(nms)

  .data <- as_list_dibble(.data)

  for (i in seq_len(size)) {
    data <- purrr::map(.data,
                       function(x) {
                         x[[i]]
                       })

    for (j in seq_nms) {
      nm <- nms[[j]]
      out[[nm]][[i]] <- data[[nm]] <- eval_tidy(dots[[j]], data)
    }
  }
  new_dibble(out, dim_names)
}



# Printing ----------------------------------------------------------------

#' @export
print.grouped_dim <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.grouped_dim <- function(x) {
  obj_sum_dibble(x)
}
