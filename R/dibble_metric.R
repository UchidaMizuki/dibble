new_dibble_metric <- function(x, dim_names) {
  class(x) <- "dibble_metric"
  attr(x, "dim_names") <- dim_names
  x
}

#' Build a dibble metric
#'
#' `dibble_metric()` constructs a metric for a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#'
#' @return A metric for a dibble.
#'
#' @export
dibble_metric <- function(x, dim_names = NULL) {
  supress_warning_broadcast(as_dibble_metric(x, dim_names))
}

supress_warning_broadcast <- function(x) {
  withCallingHandlers({
    x
  },
  warning_broadcast = function(w) {
    invokeRestart("restart_broadcast")
  })
}

#' Coerce an object to a dibble metric
#'
#' `as_dibble_metric()` turns an object into a metric for a dibble.
#'
#' @param x An object.
#' @param dim_names A list of dimension names.
#' @param ... Unused, for extensibility.
#'
#' @return A metric for a dibble.
#'
#' @export
as_dibble_metric <- function(x, ...) {
  UseMethod("as_dibble_metric")
}

#' @rdname as_dibble_metric
#' @export
as_dibble_metric.default <- function(x, dim_names, ...) {
  dim <- lengths(dim_names,
                 use.names = FALSE)
  x <- array(vec_recycle(x, prod(dim)), dim)

  new_dibble_metric(x, dim_names)
}

#' @rdname as_dibble_metric
#' @export
as_dibble_metric.array <- function(x, dim_names = NULL, ...) {
  old_names <- dimnames(x)

  if (is.null(old_names)) {
    stopifnot(
      !is.null(dim_names)
    )

    new_dibble_metric(x, dim_names)
  } else {
    x <- new_dibble_metric(x, old_names)

    if (!is.null(dim_names)) {
      x <- as_dibble_metric(x, dim_names)
    }
    x
  }
}

#' @rdname as_dibble_metric
#' @export
as_dibble_metric.table <- function(x, dim_names = NULL, ...) {
  class(x) <- NULL
  as_dibble_metric(x, dim_names)
}

#' @rdname as_dibble_metric
#' @export
as_dibble_metric.dibble_metric <- function(x, dim_names = NULL, ...) {
  if (is.null(dim_names) || identical(dimnames(x), dim_names)) {
    x
  } else {
    dim_names <- as_dim_names(dim_names, dimnames(x))
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

  x <- rep_len(list(undibble(x)), prod(new_dim))
  x <- bind_arrays(x)

  dim(x) <- unname(c(new_dim, common_dim))

  x <- new_dibble_metric(aperm(x,
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
      new_coords <- paste(c("New coords:",
                            utils::capture.output(utils::str(new_coords))[-1]),
                          collapse = "\n")
    }

    warning(warningCondition(paste0(c("Broadcasting,", new_axes, new_coords),
                                    collapse = "\n"),
                             class = "warning_broadcast"))

    x
  },
  restart_broadcast = function() {
    x
  })
}

#' Test if the object is a dibble metric
#'
#' @param x An object.
#'
#' @return A logical.
#'
#' @export
is_dibble_metric <- function(x) {
  inherits(x, "dibble_metric")
}

#' @export
as.array.dibble_metric <- function(x, ...) {
  as.array(undibble(x))
}

#' @export
as.table.dibble_metric <- function(x, ...) {
  dim_names <- dimnames(x)
  x <- undibble(x)
  dimnames(x) <- dim_names
  as.table(x)
}

#' @export
dimnames.dibble_metric <- function(x) {
  dimnames_dibble(x)
}

#' @export
`dimnames<-.dibble_metric` <- function(x, value) {
  `dimnames<-_dibble`(x, value)
}

#' @export
dim.dibble_metric <- function(x) {
  lengths(dimnames(x))
}

#' @export
nrow.dibble_metric <- function(x, ...) {
  nrow_dibble(x)
}

#' @export
ncol.dibble_metric <- function(x, ...) {
  NULL
}

#' @export
rownames.dibble_metric <- function(x, ...) {
  NULL
}

#' @export
colnames.dibble_metric <- function(x, ...) {
  NULL
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.dibble_metric <- function(x, ...) {
  as_tibble_dibble(x, ...)
}

#' @export
aperm.dibble_metric <- function(a, perm = NULL, ...) {
  aperm_dibble(a, perm, ...)
}



# Ops ---------------------------------------------------------------------

#' @export
Ops.dibble_metric <- function(e1, e2) {
  if (length(e1) != 1 && length(e2) != 1) {
    dim_names_e1 <- dimnames(e1)
    dim_names_e2 <- dimnames(e2)

    dim_names <- union_dim_names(list(dim_names_e1, dim_names_e2))

    e1 <- as_dibble_metric(e1, dim_names)
    e2 <- as_dibble_metric(e2, dim_names)
  }
  NextMethod()
}




# Math --------------------------------------------------------------------

#' @export
solve.dibble_metric <- function(a, b, ...) {
  if (is_missing(b)) {
    dim_names <- dimnames(a)
    a <- undibble(a)
    new_dibble_metric(unname(solve(a)), dim_names)
  } else {
    NextMethod()
  }
}



# Verbs -------------------------------------------------------------------

#' @importFrom dplyr slice
#' @export
slice.dibble_metric <- function(.data, ...) {
  slice_dibble(.data, ...)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dibble_metric <- function(x, ...) {
  x
}

#' @importFrom dplyr select
#' @export
select.dibble_metric <- function(.data, ...) {
  select_dibble(.data, ...)
}

#' @importFrom dplyr relocate
#' @export
relocate.dibble_metric <- function(.data, ...) {
  select_dibble(.data, ...,
                .relocate = TRUE)
}

#' @importFrom dplyr rename
#' @export
rename.dibble_metric <- function(.data, ...) {
  rename_dibble(.data, ...)
}



# Printing ----------------------------------------------------------------

#' @export
print.dibble_metric <- function(x, n = NULL, ...) {
  print_dibble(x, n)
}
