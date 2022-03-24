.onLoad <- function(...) {
  registerS3method("Ops", "ddf_col", Ops_dibble)
  registerS3method("Ops", "tbl_ddf", Ops_dibble)

  registerS3method("Math", "tbl_ddf", methods_dibble)

  registerS3method("Summary", "tbl_ddf", methods_dibble)
}
