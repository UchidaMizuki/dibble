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

  group_names <- dim_names[loc]
  dm <- lengths(group_names)
  size <- prod(dm)

  dim_names <- dim_names[-loc]

  .data <- purrr::modify(as_list_dibble(.data),
                         function(x) {
                           apply(x, loc, as.array,
                                 simplify = FALSE)
                         })

  out <- vector("list", size)

  for (i in seq_len(size)) {
    out[[i]] <- new_dibble(purrr::modify(.data,
                                         function(x) {
                                           x[[i]]
                                         }),
                           dim_names = dim_names)
  }
  new_grouped_dim(array(out, dm),
                  group_names = group_names)

  # dim_names <- dimnames(.data)
  # dm <- lengths(dim_names)
  # col_names <- names(.data)
  #
  # loc <- tidyselect::eval_select(expr(c(...)), dim_names)
  #
  # stopifnot(
  #   length(loc) < length(dim_names)
  # )
  #
  # .data <- array(purrr::flatten(.data),
  #                dim = c(dm, length(col_names)))
  #
  # margin <- c(loc, length(dm) + 1)
  #
  # group_names <- dim_names[loc]
  # dim_names <- dim_names[-loc]
  #
  # dm <- lengths(dim_names)
  # .data <- apply(.data, margin,
  #                function(x) {
  #                  x <- unlist(x,
  #                              recursive = FALSE)
  #                  array(x, dm)
  #                },
  #                simplify = FALSE)
  # .data <- apply(.data, loc,
  #                function(x) {
  #                  names(x) <- col_names
  #                  new_dibble(x, dim_names)
  #                },
  #                simplify = FALSE)
  #
  # new_grouped_dim(.data, group_names)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_dim <- function(x, ...) {
  dim_names <- dimnames(x)
  dm <- lengths(dim_names)

  group_names <- attr(x, "group_names")
  col_names <- names(x[[1]])

  x <- array(purrr::flatten(unname(x)),
             dim = c(length(col_names), lengths(group_names)))
  x <- apply(x, 1,
             function(x) {
               x <- bind_arrays(x)
               dim(x) <- dm
               x
             },
             simplify = FALSE)
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

  dots <- enquos(..., .named = TRUE)
  nms <- names(dots)

  .data <- purrr::modify(as.array(.data),
                         function(x) {
                           x <- as_list_dibble(x)

                           out <- list()
                           for (i in seq_along(nms)) {
                             out[[nms[[i]]]] <- eval_tidy(dots[[i]], c(x, out))
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
    dim(out[[i]]) <- dm
  }
  new_dibble(out, dim_names)
}



# Printing ----------------------------------------------------------------

#' @export
print.grouped_dim <- function(x, n = NULL, ...) {
  print_dibble(ungroup(x), n,
               groups = names(attr(x, "group_names")))
}
