#' Broadcast to a new dimension
#'
#' Broadcasts the dimension of the object to a new dimension.
#'
#' Operations between dibbles are automatically broadcasted, but for safety
#' reasons, warnings are issued.
#' `broadcast()` can suppress the warnings if `dim_names` matches the dimension
#' of `x`.
#'
#' @param x A dibble, vector, or array.
#' @param dim_names A character vector or list of dimension names.
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
broadcast <- function(x,
                      dim_names = NULL, ...) {
  x <- suppress_warning_broadcast(x)

  UseMethod("broadcast")
}

#' @rdname broadcast
#' @export
broadcast.default <- function(x,
                              dim_names = NULL, ...) {
  if (is.null(dimnames(x))) {
    stopifnot(
      is_dim_names(dim_names)
    )

    dim <- list_sizes_unnamed(dim_names)
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
  brdcst_dim_names <- broadcast_dim_names(x, dim_names)
  x <- broadcast_dibble(x, brdcst_dim_names$broadcast)

  new_ddf_col(x, brdcst_dim_names$new_dim_names)
}

#' @rdname broadcast
#' @export
broadcast.tbl_ddf <- function(x, dim_names, ...) {
  brdcst_dim_names <- broadcast_dim_names(x, dim_names)
  x <- broadcast_dibble(x, brdcst_dim_names$broadcast)

  new_tbl_ddf(x, brdcst_dim_names$new_dim_names)
}

broadcast_dibble <- function(x, brdcst) {
  if (is_ddf_col(x)) {
    broadcast_array(as.array(x), brdcst)
  } else if (is_tbl_ddf(x)) {
    purrr::modify(undibble(x),
                  function(x) {
                    broadcast_array(x, brdcst)
                  })
  }
}

broadcast_dim_names <- function(x, dim_names) {
  old_dim_names <- dimnames(x)
  new_dim_names <- as_dim_names(dim_names, old_dim_names)

  list(new_dim_names = new_dim_names,
       broadcast = broadcast_dim_names_warn(old_dim_names, new_dim_names))
}

broadcast_dim_names_impl <- function(old_dim_names, new_dim_names) {
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
      new_dim[new_axes %in% old_axes] <- list_sizes_unnamed(old_dim_names)

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

  list(perm = perm,
       new_dim = new_dim,
       loc = loc)
}

broadcast_dim_names_warn <- function(old_dim_names, new_dim_names) {
  out <- broadcast_dim_names_impl(old_dim_names, new_dim_names)

  if (is.null(out$new_dim)) {
    out
  } else {
    message <- broadcast_dim_names_message(old_dim_names, new_dim_names, out)

    if (vec_is_empty(message)) {
      out
    } else {
      withRestarts({
        warning(warningCondition(message,
                                 class = "warning_broadcast"))

        out
      },
      restart_broadcast = function() {
        out
      })
    }
  }
}

broadcast_dim_names_message <- function(old_dim_names, new_dim_names, brdcst) {
  new_axes <- names(new_dim_names)
  size_new_axes <- vec_size(new_axes)

  if (vec_size(old_dim_names) == size_new_axes) {
    message <- character()
  } else {
    new_axes_code <- encodeString(new_axes, quote = "\"")

    if (size_new_axes > 1L) {
      new_axes_code <- paste0("c(", paste(new_axes_code, collapse = ", "), ")")
    }

    message <- paste0("New axes, dim_names = ", new_axes_code)
  }

  new_coords <- purrr::map2(new_dim_names, brdcst$loc,
                            function(new_dim_name, loc) {
                              loc <- is.na(loc)

                              if (any(loc)) {
                                vec_slice(new_dim_name, loc)
                              } else {
                                NULL
                              }
                            })
  loc_null <- purrr::map_lgl(new_coords, is.null)
  new_coords <- new_coords[!loc_null]

  if (vec_size(new_coords) >= 1L) {
    message <- paste0(c(message,
                        "New coordinates, ",
                        utils::capture.output(utils::str(new_coords))[-1]),
                      collapse = "\n")
  }

  if (vec_is_empty(message)) {
    character()
  } else {
    paste0(c("Broadcasting,", message),
           collapse = "\n")
  }
}

suppress_warning_broadcast <- function(x) {
  withCallingHandlers(x,
                      warning_broadcast = function(w) {
                        invokeRestart("restart_broadcast")
                      })
}

# Broadcast an array based on the results of `broadcast_dim_names_impl()`.
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
