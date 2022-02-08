.onLoad <- function(...) {
  registerS3method("Ops", "dibble", Ops_dibble)
  registerS3method("Ops", "grouped_dibble", Ops_dibble)
  registerS3method("Ops", "dibble_measure", Ops_dibble)

  registerS3method("Math", "dibble", methods_dibble)
  registerS3method("Math", "grouped_dibble", methods_dibble)

  registerS3method("Summary", "dibble", methods_dibble)
  registerS3method("Summary", "grouped_dibble", methods_dibble)
}

# .onAttach <- function(...) {
#   setOldClass("dibble")
#   setOldClass("grouped_dibble")
# }
