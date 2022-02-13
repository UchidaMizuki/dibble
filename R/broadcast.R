#' Broadcast to a new dimension
#'
#' Broadcasts the dimension of the object to a new dimension.
#' This function is generic.
#'
#' @param x A dibble, vector, or array.
#' @param dim_names A named list of dimension names.
#' @param ... Unused, for extensibility.
#'
#' @return A dibble.
#'
#' @examples
#' x <- broadcast(1:2,
#'                list(axis1 = letters[1:2]))
#' y <- broadcast(1:3,
#'                list(axis2 = letters[1:3]))
#' broadcast(x * y, c("axis1", "axis2"))
#'
#' @export
broadcast <- function(x, dim_names, ...) {
  x <- suppress_warning_broadcast(x)

  UseMethod("broadcast")
}

#' @rdname broadcast
#' @export
broadcast.default <- function(x, dim_names, ...) {
  if (is.null(dimnames(x))) {
    stopifnot(
      is_dim_names(dim_names)
    )

    dim <- list_sizes(dim_names)
    x <- array(vec_recycle(x, prod(dim)),
               dim = dim)

    new_ddf_col(x, dim_names)
  } else {
    broadcast(as_dibble(x), dim_names)
  }
}

#' @rdname broadcast
#' @export
broadcast.ddf_col <- function(x, dim_names, ...) {
  brdcst <- broadcast_dibble(x, dim_names)
  x <- broadcast_array(as.array(x), brdcst$broadcast)

  new_ddf_col(x, brdcst$new_dim_names)
}

#' @rdname broadcast
#' @export
broadcast.tbl_ddf <- function(x, dim_names, ...) {
  brdcst <- broadcast_dibble(x, dim_names)
  x <- lapply(undibble(x),
              function(x) {
                broadcast_array(x, brdcst$broadcast)
              })

  new_tbl_ddf(x, brdcst$new_dim_names)
}

#' @rdname broadcast
#' @export
broadcast.grouped_ddf <- function(x, dim_names, ...) {
  axes <- group_vars(x)
  x <- broadcast(ungroup(x), dim_names, ...)
  group_by(x, dplyr::all_of(axes))
}

broadcast_dibble <- function(x, dim_names) {
  old_dim_names <- dimnames(x)
  new_dim_names <- as_dim_names(dim_names, old_dim_names)

  list(new_dim_names = new_dim_names,
       broadcast = broadcast_dim_names(old_dim_names, new_dim_names))
}

broadcast_dim_names <- function(old_dim_names, new_dim_names) {
  if (identical(old_dim_names, new_dim_names)) {
    perm <- NULL
    new_dim <- NULL
    loc <- NULL
  } else {
    old_axes <- names(old_dim_names)
    new_axes <- names(new_dim_names)
    stopifnot(
      old_axes %in% new_axes
    )

    # Transposition
    perm <- vec_match(new_axes[new_axes %in% old_axes], old_axes)
    old_dim_names <- old_dim_names[perm]

    if (identical(old_dim_names, new_dim_names)) {
      new_dim <- NULL
      loc <- NULL
    } else {
      # Set dimensions
      new_dim <- vec_rep(1, vec_size(new_axes))
      new_dim[new_axes %in% old_axes] <- list_sizes(old_dim_names)

      # Subsetting
      old_dim_names <- old_dim_names[new_axes]
      loc <- vec_init_along(list(), new_axes)

      for (axis in vec_seq_along(new_axes)) {
        old_dim_name <- old_dim_names[[axis]]
        new_dim_name <- new_dim_names[[axis]]

        if (is.null(old_dim_name)) {
          loc[[axis]] <- vec_rep(1, vec_size(new_dim_name))
        } else {
          loc[[axis]] <- vec_match(new_dim_name, old_dim_name)
        }
      }
    }
  }

  out <- list(perm = perm,
              new_dim = new_dim,
              loc = loc)

  if (is.null(new_dim)) {
    out
  } else {
    withRestarts({
      warning(warningCondition(paste0(c("Broadcasting,",
                                        utils::capture.output(utils::str(new_dim_names))[-1]),
                                      collapse = "\n"),
                               class = "warning_broadcast"))

      out
    },
    restart_broadcast = function() {
      out
    })
  }
}

suppress_warning_broadcast <- function(x) {
  withCallingHandlers(x,
                      warning_broadcast = function(w) {
                        invokeRestart("restart_broadcast")
                      })
}

# Broadcast an array based on the results of `broadcast_dim_names()`.
broadcast_array <- function(x, brdcst) {
  perm <- brdcst$perm

  if (is.null(perm)) {
    x
  } else {
    x <- aperm(x, perm)

    new_dim <- brdcst$new_dim
    loc <- brdcst$loc

    if (is.null(new_dim)) {
      x
    } else {
      dim(x) <- new_dim
      exec(`[`, x, !!!loc,
           drop = FALSE)
    }
  }
}
