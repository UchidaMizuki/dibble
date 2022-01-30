#' @export
new_grouped_dibble <- function(x, group_dim_names, ...,
                               class = character()) {
  structure(x,
            group_dim_names = group_dim_names, ...,
            class = c(class, "grouped_dibble"))
}

group_dim_names <- function(x) {
  attr(x, "group_dim_names")
}

#' @importFrom dplyr group_by
#' @export
group_by.dibble <- function(.data, ...) {
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
                                 new_dibble_measure(x, dim_names)
                               },
                               simplify = FALSE)
                    array(x, group_dim)
                  })
  new_grouped_dibble(.data,
                     group_dim_names = group_dim_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dibble <- function(x, ...) {
  dim_names <- dimnames(x)
  axes <- names(dim_names)
  dim <- list_sizes(dim_names)

  group_dim_names <- group_dim_names(x)
  group_axes <- names(group_dim_names)

  loc <- vec_match(group_axes, axes)
  dim <- c(dim[-loc], dim[loc])

  axes <- seq_along(axes)
  perm <- c(setdiff(axes, loc), loc)
  perm <- vec_match(axes, perm)

  x <- undibble(x)
  col_names <- names(x)
  x <- lapply(x,
              function(x) {
                x <- array(rlang::exec(c, !!!x),
                           dim = dim)
                aperm(x, perm)
              })
  names(x) <- col_names
  new_dibble(x, dim_names)
}

#' Test if the object is a grouped dibble
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_grouped_dibble <- function(x) {
  inherits(x, "grouped_dibble")
}

#' @export
dimnames.grouped_dibble <- function(x) {
  group_dim_names <- group_dim_names(x)

  x <- undibble(x)
  dim_names <- dimnames(x[[1]][[1]])

  c(group_dim_names, dim_names)
}

#' @export
`dimnames<-.grouped_dibble` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.grouped_dibble <- function(x) {
  list_sizes(dimnames(x))
}

#' @export
nrow.grouped_dibble <- function(x, ...) {
  nrow_dibble(x)
}

#' @export
ncol.grouped_dibble <- function(x, ...) {
  length(colnames(x))
}

#' @export
rownames.grouped_dibble <- function(x, ...) {
  NULL
}

#' @export
colnames.grouped_dibble <- function(x, ...) {
  names(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.grouped_dibble <- function(x, ...) {
  as_tibble(ungroup(x), ...)
}



# Subsetting --------------------------------------------------------------

#' @export
`[.grouped_dibble` <- function(x, i) {
  new_grouped_dibble(NextMethod(), group_dim_names(x))
}

#' @export
`[<-.grouped_dibble` <- function(x, i, value) {
  `[<-_dibble`(x, i, value)
}

#' @export
`[[.grouped_dibble` <- function(x, i) {
  x <- ungroup(x)
  x[[i]]
}

#' @export
`[[<-.grouped_dibble` <- function(x, i, value) {
  `[[<-_dibble`(x, i, value)
}

#' @export
`$.grouped_dibble` <- function(x, i) {
  x <- ungroup(x)
  x[[i]]
}

#' @export
`$<-.grouped_dibble` <- function(x, i, value) {
  `$<-_dibble`(x, i, value)
}



# Ops ---------------------------------------------------------------------

#' @export
Ops.grouped_dibble <- function(e1, e2) {
  e1 <- as_dibble_measure(e1)

  if (is_dibble(e2) || is_grouped_dibble(e2)) {
    e2 <- as_dibble_measure(e2)
  }
  NextMethod()
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.grouped_dibble <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom  dplyr mutate
#' @export
mutate.grouped_dibble <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)
  seq_nms <- seq_along(nms)

  group_dim_names <- group_dim_names(.data)
  group_dim <- list_sizes(group_dim_names)
  size <- prod(group_dim)

  .data <- undibble(.data)
  dim_names <- dimnames(.data[[1]][[1]])

  out <- .data
  out[setdiff(nms, names(out))] <- list(array(list(), group_dim))

  for (i in seq_len(size)) {
    data <- lapply(.data,
                   function(x) {
                     x[[i]]
                   })

    for (j in seq_nms) {
      nm <- nms[[j]]
      out[[nm]][[i]] <- data[[nm]] <- dibble_measure(eval_tidy(dots[[j]], data), dim_names)
    }
  }
  new_grouped_dibble(out, group_dim_names)
}

#' @importFrom dplyr summarise
#' @export
summarise.grouped_dibble <- function(.data, ...) {
  dim_names <- group_dim_names(.data)
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
      out[[nm]][[i]] <- data[[nm]] <- eval_tidy(dots[[j]], data)
    }
  }
  new_dibble(out, dim_names)
}

#' @importFrom dplyr select
#' @export
select.grouped_dibble <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.grouped_dibble <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.grouped_dibble <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.grouped_dibble <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
