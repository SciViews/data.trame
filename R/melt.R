
#' Fast melt for data.trame
#'
#' `melt() is reshaping wide-to-long, notb unlike `tidyr::pivot_longer()` for
#' data.trame objects. See [data.table::melt()] for explanations.
#'
#' @param data A data.trame object.
#' @param id.vars Vector of `id` variables.
#' @param measure.vars Measure variables for melting.
#' @param variable.name Name (default `variable`) of output column containing
#'   the information about melted columns.
#' @param value.name Name for the molten data values column(s).
#' @param ... Arguments passed to other methods.
#' @param na.rm Should `NA` values be removed? Default is `FALSE`.
#' @param variable.factor If `TRUE`, the variable column is converted to a
#'   factor, otherwise, it is a character column.
#' @param value.factor If `TRUE`, the value column is converted to a factor,
#'   else, it is left unchanged.
#' @param verbose `TRUE` turns on status and information messages.
#'
#' @returns An unkeyed data.trame containing the molten data.
#' @export

#' @seealso [data.table::melt()], [dcast()]
#'
#' @examples
#' # Adapted from first example of ?melt.data.table
#' set.seed(45)
#' library(data.trame)
#' dtrm <- data.trame(
#'   i_1 = c(1:5, NA),
#'   n_1 = c(NA, 6, 7, 8, 9, 10),
#'   f_1 = factor(sample(c(letters[1:3], NA), 6L, TRUE)),
#'   f_2 = ordered(c("z", "a", "x", "c", "x", "x")),
#'   c_1 = sample(c(letters[1:3], NA), 6L, TRUE),
#'   c_2 = sample(c(LETTERS[1:2], NA), 6L, TRUE),
#'   d_1 = as.Date(c(1:3,NA,4:5), origin = "2013-09-01"),
#'   d_2 = as.Date(6:1, origin = "2012-01-01")
#' )
#' # add a couple of list cols
#' dtrm$l_1 <- dtrm[, ~list(c = list(rep(i_1, sample(5, 1L)))), by = ~i_1]$c
#' dtrm$l_2 <- dtrm[, ~list(c = list(rep(c_1, sample(5, 1L)))), by = ~i_1]$c
#'
#' # id.vars, measure.vars as character/integer/numeric vectors
#' melt(dtrm, id.vars = 1:2, measure.vars = "f_1")
melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
  UseMethod("melt", data)
}

#' @export
#' @rdname melt
melt.data.trame <- function(data, id.vars, measure.vars,
    variable.name = "variable", value.name = "value", ..., na.rm = FALSE,
    variable.factor = TRUE, value.factor = FALSE,
    verbose = getOption("datatable.verbose")) {
  (setattr(NextMethod("melt", data), 'class',
    c('data.trame', 'data.table', 'data.frame')))
}

#' @export
#' @noRd
melt.data.table <- data.table::melt.data.table
