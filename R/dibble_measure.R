new_dibble_measure <- function(x, dim_names) {
  class(x) <- "dibble_measure"
  attr(x, "dim_names") <- dim_names
  x
}

#' @export
dibble_measure <- function(x, dim_names = NULL) {
  withCallingHandlers({
    as_dibble_measure(x, dim_names)
  },
  warning_broadcast = function(w) {
    invokeRestart("restart_broadcast")
  })
}

#' @export
as_dibble_measure <- function(x, ...) {
  UseMethod("as_dibble_measure")
}

#' @export
as_dibble_measure.default <- function(x, dim_names, ...) {
  dim <- lengths(dim_names)
  x <- array(vec_recycle(x, prod(dim)), dim)

  new_dibble_measure(x, dim_names)
}

#' @export
as_dibble_measure.array <- function(x, dim_names = NULL, ...) {
  if (is.null(dim_names)) {
    dim_names <- dimnames(x)
  } else {
    stopifnot(
      all(dim(x) == lengths(dim_names))
    )
  }

  dimnames(x) <- NULL
  new_dibble_measure(x, dim_names)
}

#' @export
as_dibble_measure.dibble_measure <- function(x, dim_names = NULL, ...) {
  if (is.null(dim_names) || identical(dimnames(x), dim_names)) {
    x
  } else {
    broadcast(x, dim_names)
  }
}

broadcast <- function(x, dim_names) {
  old_names <- dimnames(x)
  old_axes <- names(old_names)

  axes <- names(dim_names)

  stopifnot(
    all(old_axes %in% axes)
  )

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
  x <- new_dibble_measure(aperm(x,
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

#' @export
is_dibble_measure <- function(x) {
  inherits(x, "dibble_measure")
}

#' @export
as.array.dibble_measure <- function(x, ...) {
  class(x) <- NULL
  attr(x, "dim_names") <- NULL
  NextMethod()
}

#' @export
as.table.dibble_measure <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- as.array(x)
  dimnames(x) <- dim_names
  as.table(x)
}

#' @export
dimnames.dibble_measure <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.dibble_measure` <- function(x, value) {
  assign_dimnames_dibble(x, value)
}

#' @export
dim.dibble_measure <- function(x) {
  lengths(dimnames(x))
}

#' @export
nrow.dibble_measure <- function(x) {
  nrow_dibble(x)
}

#' @export
ncol.dibble_measure <- function(x) {
  1L
}

#' @export
rownames.dibble_measure <- function(x, ...) {
  NULL
}

#' @export
colnames.dibble_measure <- function(x, ...) {
  NULL
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dibble_measure <- function(x, ...) {
  as_tibble_dibble(x, ...)
}



# Ops ---------------------------------------------------------------------

#' @export
Ops.dibble_measure <- function(e1, e2) {
  if (length(e1) != 1 && length(e2) != 1) {
    dim_names_e1 <- dimnames(e1)
    dim_names_e2 <- dimnames(e2)

    axes <- unique(c(names(dim_names_e1), names(dim_names_e2)))
    dim_names <- purrr::map(axes,
                            function(x) {
                              unique(c(dim_names_e1[[x]], dim_names_e2[[x]]))
                            })
    names(dim_names) <- axes

    e1 <- as_dibble_measure(e1, dim_names)
    e2 <- as_dibble_measure(e2, dim_names)
  }
  NextMethod()
}




# Math --------------------------------------------------------------------

#' @export
solve.dibble_measure <- function(a, b, ...) {
  if (is_missing(b)) {
    dim_names <- dimnames(a)
    a <- as.array(a)
    new_dibble_measure(solve(a), dim_names)
  } else {
    NextMethod()
  }
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.dibble_measure <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dibble_measure <- function(x, ...) {
  x
}



# Printing ----------------------------------------------------------------

#' @export
print.dibble_measure <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
