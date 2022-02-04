#' @export
broadcast <- function(x, dim_names, ...) {
  x <- supress_warning_broadcast(x)

  UseMethod("broadcast")
}

#' @export
broadcast.default <- function(x, dim_names, ...) {
  stopifnot(
    rlang::is_named(dim_names)
  )
  dim <- list_sizes(dim_names)
  x <- array(vec_recycle(x, prod(dim)),
             dim)
  new_dibble_measure(x, dim_names)
}

#' @export
broadcast.dibble <- function(x, dim_names, ...) {
  brdcst <- broadcast_dibble(x, dim_names)
  x <- lapply(undibble(x),
              function(x) {
                broadcast_array(x, brdcst$broadcast)
              })

  new_dibble(x, brdcst$new_dim_names)
}

#' @export
broadcast.dibble_measure <- function(x, dim_names, ...) {
  brdcst <- broadcast_dibble(x, dim_names)
  x <- broadcast_array(as.array(x), brdcst$broadcast)

  new_dibble_measure(x, brdcst$new_dim_names)
}

# FIXME?: Keep grouping or not? Now, grouping isn't kept.
#' @export
broadcast.grouped_dibble <- function(x, dim_names, ...) {
  broadcast(ungroup(x), dim_names)
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
      # names(new_dim) <- new_axes
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

  if (is.null(perm)) {
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

supress_warning_broadcast <- function(x) {
  withCallingHandlers({
    x
  },
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
      rlang::exec(`[`, x, !!!loc)
    }
  }
}
