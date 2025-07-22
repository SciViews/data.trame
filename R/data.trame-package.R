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
#'
#' - Methods for data.frame objects.

## usethis namespace: start
#' @importFrom checkmate check_character
#' @importFrom collapse any_duplicated fnrow qDT seq_row setv ss
#' @importFrom data.table %chin% as.data.table copy cube dcast dcast.data.table groupingsets is.data.table key let melt melt.data.table rollup set setalloccol setattr setDT setnames setkeyv .N .I `:=`
#' @importFrom pillar ctl_new_rowid_pillar new_pillar new_pillar_shaft pillar_component tbl_nrow
#' @importFrom rlang check_dots_empty0 f_lhs f_rhs
#' @importFrom tibble add_column as_tibble obj_sum size_sum tbl_sum tibble
#' @importFrom utils getS3method head str tail
## usethis namespace: end
"_PACKAGE"
