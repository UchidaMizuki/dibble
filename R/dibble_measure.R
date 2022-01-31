#' @export
new_dibble_measure <- function(x, dim_names, ...,
                               class = character()) {
  structure(x,
            dim_names = dim_names, ...,
            class = c(class, "dibble_measure"))
}

#' Build a dibble measure
#'
#' `dibble_measure()` constructs a measure for a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#'
#' @return A measure for a dibble.
#'
#' @export
dibble_measure <- function(x, dim_names = NULL) {
  supress_warning_broadcast(as_dibble_measure(x, dim_names))
}

# supress_warning_broadcast <- function(x) {
#   withCallingHandlers({
#     x
#   },
#   warning_broadcast = function(w) {
#     invokeRestart("restart_broadcast")
#   })
# }

#' Coerce an object to a dibble measure
#'
#' `as_dibble_measure()` turns an object into a measure for a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#' @param ... Unused, for extensibility.
#'
#' @return A measure for a dibble.
#'
#' @export
as_dibble_measure <- function(x, ...) {
  UseMethod("as_dibble_measure")
}

#' @rdname as_dibble_measure
#' @export
as_dibble_measure.dibble_measure <- function(x, ...) {
  x
}

#' @rdname as_dibble_measure
#' @export
as_dibble_measure.dibble <- function(x, ...) {
  stopifnot(
    ncol(x) == 1L
  )
  x[[1L]]
}

#' @rdname as_dibble_measure
#' @export
as_dibble_measure.grouped_dibble <- function(x, ...) {
  stopifnot(
    ncol(x) == 1L
  )
  x[[1L]]
}

#' @export
broadcast <- function(x, dim_names) {
  old_dim_names <- dimnames(x)

  if (identical(old_dim_names, dim_names)) {
    x
  } else {
    old_axes <- names(old_dim_names)
    new_axes <- names(dim_names)

    stopifnot(
      old_axes %in% new_axes
    )

    old_axes <- intersect(new_axes, old_axes)
    old_dim_names <- old_dim_names[old_axes]
    old_dim <- list_sizes(old_dim_names)

    # permutation
    x <- aperm(x, old_axes)

    if (identical(old_dim_names, dim_names)) {
      x
    } else {
      x <- as.array(x)

      # set dimensions
      new_dim <- vec_rep(1, vec_size(new_axes))
      names(new_dim) <- new_axes
      new_dim[old_axes] <- old_dim

      dim(x) <- new_dim

      # subsetting
      old_dim_names <- old_dim_names[new_axes]
      loc <- vec_init_along(list(), new_axes)

      for (axis in vec_seq_along(new_axes)) {
        dim_name <- dim_names[[axis]]
        old_dim_name <- old_dim_names[[axis]]

        if (is.null(old_dim_name)) {
          loc[[axis]] <- vec_rep(1, vec_size(dim_name))
        } else {
          loc[[axis]] <- vec_match(dim_name, old_dim_name)
        }
      }

      x <- new_dibble_measure(rlang::exec(`[`, x, !!!loc),
                              dim_names)

      withRestarts({
        # Warning
        warning(warningCondition(paste0(c("Broadcasting,",
                                          utils::capture.output(utils::str(dim_names))[-1]),
                                        collapse = "\n"),
                                 class = "warning_broadcast"))

        x
      },
      restart_broadcast = function() {
        x
      })
    }
  }
}

#' Test if the object is a dibble measure
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble_measure <- function(x) {
  inherits(x, "dibble_measure")
}

#' @export
as.array.dibble_measure <- function(x, ...) {
  as.array(undibble(x))
}

#' @export
as.table.dibble_measure <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- undibble(x)
  dimnames(x) <- dim_names
  as.table(x)
}

#' @export
dimnames.dibble_measure <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.dibble_measure` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.dibble_measure <- function(x) {
  list_sizes(dimnames(x))
}

#' @export
nrow.dibble_measure <- function(x, ...) {
  nrow_dibble(x)
}

#' @export
ncol.dibble_measure <- function(x, ...) {
  NULL
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

#' @export
aperm.dibble_measure <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}

#' @export
apply.dibble_measure <- function(x, margin, fun, ...) {
  dim_names <- dimnames(x)

  if (is.character(margin)) {
    margin <- vec_match(margin, names(dim_names))
  }

  x <- apply(as.array(x), margin, fun, ...)
  new_dibble_measure(x, dim_names[margin])
}


# Ops ---------------------------------------------------------------------

#' @export
Ops_dibble <- function(e1, e2) {
  if (is_dibble(e1) || is_grouped_dibble(e1)) {
    e1 <- as_dibble_measure(e1)
  }

  if (is_dibble(e2) || is_grouped_dibble(e2)) {
    e2 <- as_dibble_measure(e2)
  }

  if (!rlang::is_scalar_vector(e1) && !rlang::is_scalar_vector(e2)) {
    dim_names <- union_dim_names(dimnames(e1), dimnames(e2))

    e1 <- broadcast(e1, dim_names)
    e2 <- broadcast(e2, dim_names)
  }
  NextMethod()
}




# Math --------------------------------------------------------------------

#' @export
solve.dibble_measure <- function(a, b, ...) {
  if (is_missing(b)) {
    dim_names <- dimnames(a)
    a <- undibble(a)
    new_dibble_measure(unname(solve(a)), dim_names)
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

#' @importFrom dplyr select
#' @export
select.dibble_measure <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.dibble_measure <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.dibble_measure <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.dibble_measure <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
