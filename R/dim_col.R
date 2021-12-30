new_dim_col <- function(x, dim_names) {
  class(x) <- "dim_col"
  attr(x, "dim_names") <- dim_names
  x
}

#' @export
as_dim_col <- function(x, ...) {
  UseMethod("as_dim_col")
}

#' @export
as_dim_col.default <- function(x, dim_names, ...) {
  dim <- lengths(dim_names)
  x <- array(vec_recycle(x, prod(dim)), dim)
  new_dim_col(x, dim_names)
}

#' @export
as_dim_col.dim_col <- function(x, ...) {
  x
}

#' @export
is_dim_col <- function(x) {
  inherits(x, "dim_col")
}

#' @export
as.array.dim_col <- function(x, ...) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  NextMethod()
}

#' @export
as.table.dim_col <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- as.array(x)
  dimnames(x) <- dim_names
  as.table(x)
}

#' @export
dimnames.dim_col <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.dim_col` <- function(x, value) {
  assign_dimnames_dibble(x, value)
}

#' @export
dim.dim_col <- function(x) {
  lengths(dimnames(x))
}

#' @export
nrow.dim_col <- function(x) {
  nrow_dibble(x)
}

#' @export
ncol.dim_col <- function(x) {
  1L
}

#' @export
rownames.dim_col <- function(x, ...) {
  NULL
}

#' @export
colnames.dim_col <- function(x, ...) {
  NULL
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dim_col <- function(x, ...) {
  as_tibble_dibble(x, ...)
}



# Ops ---------------------------------------------------------------------

#' @export
Ops.dim_col <- function(e1, e2) {
  if (length(e1) != 1 && length(e2) != 1) {
    dim_names_e1 <- dimnames(e1)
    dim_names_e2 <- dimnames(e2)

    axes <- unique(c(names(dim_names_e1), names(dim_names_e2)))
    dim_names <- purrr::map(axes,
                            function(x) {
                              unique(c(dim_names_e1[[x]], dim_names_e2[[x]]))
                            })
    names(dim_names) <- axes

    e1 <- broadcast(e1, dim_names)
    e2 <- broadcast(e2, dim_names)
  }
  NextMethod()
}

broadcast <- function(x, dim_names) {
  old_names <- dimnames(x)

  if (identical(old_names, dim_names)) {
    x
  } else {
    old_axes <- names(old_names)
    axes <- names(dim_names)

    # Broadcast coordinates
    common_names <- dim_names[old_axes]
    common_dim <- lengths(common_names)

    new_coords <- purrr::modify2(common_names, old_names, setdiff)
    names(new_coords) <- old_axes

    x <- slice(x, !!!purrr::modify2(common_names, old_names, vec_match))
    dimnames(x) <- common_names

    # Broadcast axes
    new_axes <- setdiff(axes, old_axes)
    new_names <- dim_names[new_axes]
    new_dim <- lengths(new_names)

    x <- bind_arrays(rep_len(list(as.array(x)),
                             length.out = prod(new_dim)))
    dim(x) <- c(new_dim, common_dim)
    x <- new_dim_col(aperm(x,
                           perm = vec_match(axes, c(new_axes, old_axes))),
                     dim_names = dim_names)

    withRestarts({
      # Warning
      if (vec_is_empty(new_axes)) {
        new_axes <- NULL
      } else {
        new_axes <- commas(new_axes)
        new_axes <- paste("New axes:", new_axes)
      }

      new_coords <- new_coords[lengths(new_coords) > 0]
      if (vec_is_empty(new_coords)) {
        new_coords <- NULL
      } else {
        new_coords <- purrr::map_chr(new_coords, commas)
        new_coords <- data.frame(axis = names(new_coords),
                                 coord = unname(new_coords))
        new_coords <- paste(c("New coords:",
                              capture.output(new_coords)),
                            collapse = "\n")
      }

      warning(warningCondition(paste0(c("Broadcasting,", new_axes, new_coords),
                                      collapse = "\n"),
                               class = "warning_broadcast"))

      x
    },
    restart_broadcast = function() x)
  }
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.dim_col <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dim_col <- function(x, ...) {
  x
}



# Printing ----------------------------------------------------------------

#' @export
print.dim_col <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}

#' @importFrom pillar obj_sum
#' @export
obj_sum.dim_col <- function(x) {
  dim_sum(x)
}
