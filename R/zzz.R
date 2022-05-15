.onLoad <- function(...) {
  as_dim_names <<- memoise::memoise(as_dim_names)
  union_dim_names <<- memoise::memoise(union_dim_names)
  broadcast_dim_names_impl <<- memoise::memoise(broadcast_dim_names_impl)
  broadcast_dim_names_message <<- memoise::memoise(broadcast_dim_names_message)

  registerS3method("Ops", "ddf_col", Ops_dibble)
  registerS3method("Ops", "tbl_ddf", Ops_dibble)

  registerS3method("Math", "tbl_ddf", methods_dibble)

  registerS3method("Summary", "tbl_ddf", methods_dibble)
}
