#' @export
new_grouped_dim <- function(x, group_names, ..., class = character()) {
  structure(x,
            group_names = group_names,
            ...,
            class = c(class, "grouped_dim"))
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_dim <- function(.data, ...) {
  dim_names <- dimnames(.data)
  loc <- tidyselect::eval_select(expr(c(...)), dim_names)

  stopifnot(
    length(loc) < length(dim_names)
  )

  perm <- c(loc, setdiff(seq_along(dim_names), loc))

  group_names <- dim_names[loc]
  group_dim <- lengths(group_names)
  size <- prod(group_dim)

  dim_names <- dim_names[-loc]
  dim <- lengths(dim_names)

  dim_data <- c(size, lengths(dim_names))
  .data <- purrr::modify(as_list_dibble(.data),
                         function(x) {
                           x <- aperm(x, perm)
                           dim(x) <- dim_data
                           vec_chop(x)
                         })

  out <- vector("list", size)

  for (i in seq_len(size)) {
    out[[i]] <- new_dibble(purrr::modify(.data,
                                         function(x) {
                                           x <- x[[i]]
                                           dim(x) <- dim
                                           x
                                         }),
                           dim_names = dim_names)
  }
  new_grouped_dim(array(out, group_dim),
                  group_names = group_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  dim <- lengths(dim_names)

  group_names <- attr(x, "group_names")
  x <- as_list_dibble(x)

  col_names <- names(x[[1]])
  size <- length(col_names)

  out <- vector("list", size)
  names(out) <- col_names

  for (i in seq_len(size)) {
    out[[i]] <- bind_arrays(purrr::modify(x,
                                          function(x) {
                                            as_list_dibble(x)[[i]]
                                          }))
    dim(out[[i]]) <- dim
  }
  new_dibble(out, dim_names)
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
  as_tibble_dibble(x, ...)
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
  dim <- lengths(dim_names)

  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)

  .data <- purrr::modify(as.array(.data),
                         function(x) {
                           x <- as_list_dibble(x)

                           out <- list()
                           data <- x

                           for (i in seq_along(nms)) {
                             nm <- nms[i]
                             out[[nm]] <- data[[nm]] <- eval_tidy(dots[[i]], data)
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
                                     }),
                       recursive = FALSE)
    dim(out[[i]]) <- dim
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
