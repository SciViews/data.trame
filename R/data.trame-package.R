#' A better 'data.frame' for 'SciViews::R'
#'
#' The 'data.trame' object is an hybrid between 'data.table', tibble' and
#' 'data.frame'. It enhances the 'data.frame' with the speed of 'data.table' and
#' the nice features of 'tibble' (petty printing, stricter rules...).
#'
#' @section Important functions:
#'
#' - [data.trame()] to construct a data.trame object,
#'
#' - [as.data.trame()] to coerce into a data.trame object.
#'
#' - [is.data.trame()] to test for data.trame objects.

## usethis namespace: start
#' @importFrom collapse qDT seq_row setv ss
#' @importFrom data.table %chin% as.data.table copy dcast dcast.data.table is.data.table key let melt melt.data.table set setalloccol setattr setDT setkeyv .N .I `:=`
#' @importFrom pillar tbl_nrow
#' @importFrom rlang check_dots_empty0 f_lhs f_rhs
#' @importFrom tibble as_tibble obj_sum size_sum tbl_sum tibble
#' @importFrom utils getS3method str
## usethis namespace: end
"_PACKAGE"
