
#' @export
ifelse <- function(test, yes, no, ...) {
  UseMethod("ifelse")
}

#' @export
ifelse.default <- function(test, yes, no, ...) {
  base::ifelse(test, yes, no)
}

#' @export
ifelse.dibble <- function(test, yes, no, ...) {
  ifelse_dibble(test, yes, no)
}

#' @export
ifelse.grouped_dibble <- function(test, yes, no, ...) {
  ifelse_dibble(test, yes, no)
}

ifelse_dibble <- function(test, yes, no) {
  ifelse(as_dibble_measure(test), yes, no)
}

#' @export
ifelse.dibble_measure <- function(test, yes, no, ...) {
  dim_names <- union_dim_names(dimnames(test), dimnames(yes), dimnames(no))
  test <- as.array(broadcast(test, dim_names))
  yes <- as.array(dibble_measure(yes, dim_names))
  no <- as.array(dibble_measure(no, dim_names))

  new_dibble_measure(ifelse(test, yes, no),
                     dim_names)
}
