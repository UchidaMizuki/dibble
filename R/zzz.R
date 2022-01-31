.onLoad <- function(...) {
  registerS3method("Ops", "dibble", Ops_dibble)
  registerS3method("Ops", "grouped_dibble", Ops_dibble)
  registerS3method("Ops", "dibble_measure", Ops_dibble)
}
