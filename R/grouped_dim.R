new_grouped_dim <- function(x, group_names) {
  structure(x,
            class = "grouped_dim",
            group_names = group_names)
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  dim_names <- dimnames(.data)
  loc <- tidyselect::eval_select(rlang::expr(c(...)), dim_names)

  stopifnot(
    length(loc) < length(dim_names)
  )

  group_names <- dim_names[loc]
  dim_names <- dim_names[-loc]

  dm <- lengths(group_names)
  .data <- purrr::modify(as.list(.data),
                         function(x) {
                           apply(x, loc, as.array,
                                 simplify = FALSE)
                         })

  size <- prod(dm)
  out <- vector("list", size)

  for (i in seq_len(size)) {
    out[[i]] <- new_tbl_dim(purrr::modify(.data,
                                          function(x) x[[i]]),
                            dim_names = dim_names)
  }

  new_grouped_dim(array(out,
                        dim = dm),
                  group_names = group_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  dm <- lengths(dim_names)

  col_names <- names(x[[1]])

  size <- length(col_names)
  out <- vector("list", size)
  names(out) <- col_names

  for (i in seq_len(size)) {
    out[[i]] <- bind_arrays(purrr::modify(as.array(x),
                                          function(x) {
                                            as.array(x[[i]])
                                          }))
    dim(out[[i]]) <- dm
  }

  new_tbl_dim(out,
              dim_names = dim_names)
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
  c(attr(x, "group_names"), dimnames(x[[1]][[1]]))
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
  ncol(x[[1]])
}

#' @export
rownames.grouped_dim <- function(x, ...) {
  NULL
}

#' @export
colnames.grouped_dim <- function(x, ...) {
  names(x[[1]])
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.grouped_dim <- function(x, ...) {
  as_tibble_dibble(ungroup(x), ...)
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
  new_grouped_dim(purrr::modify(as.array(.data),
                                function(x) {
                                  mutate(x, ...)
                                }),
                  group_names = attr(.data, "group_names"))
}

#' @importFrom dplyr summarise
#' @export
summarise.grouped_dim <- function(.data, ...) {
  dim_names <- attr(.data, "group_names")
  dm <- lengths(dim_names)

  dots <- rlang::enquos(..., .named = TRUE)
  nms <- names(dots)

  .data <- purrr::modify(as.array(.data),
                         function(x) {
                           x <- as.list(x)

                           out <- list()
                           for (i in seq_along(nms)) {
                             out[[nms[[i]]]] <- rlang::eval_tidy(dots[[i]], c(x, out))
                           }
                           out
                         })

  size <- length(nms)
  out <- vector("list", size)
  names(out) <- nms

  for (i in seq_len(size)) {
    out[[i]] <- unlist(purrr::modify(.data,
                                     function(x) {
                                       x[[i]]
                                     }))
    dim(out[[i]]) <- dm
  }
  new_tbl_dim(out,
              dim_names = dim_names)
}



# Printing ----------------------------------------------------------------

#' @export
print.grouped_dim <- function(x, n = NULL, ...) {
  print_dibble(ungroup(x), n,
               groups = names(attr(x, "group_names")))
}
