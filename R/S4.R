setOldClass("ddf_col")
setOldClass("tbl_ddf")
setOldClass("grouped_ddf")

setClassUnion("dibble", c("ddf_col", "tbl_ddf", "grouped_ddf", "array", "vector"))
