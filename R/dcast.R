# This function is not exported from data.table -> copied here
guess <- function (x) {
  if ("value" %chin% names(x))
    return("value")
  if ("(all)" %chin% names(x))
    return("(all)")
  var = names(x)[ncol(x)]
  message("Using '%s' as value column. Use 'value.var' to override", var)
  var
}

#' Fast dcast for data.trame
#'
#' `dcast()` transforms a data.trame from long to wide format, a little bit like
#' `tidyr::pivot_wider()`. See [data.table::dcast()] for explanations.
#'
#' @param data A data.trame object.
#' @param formula A formula LHS ~ RHS, see [data.table::dcast()]
#'   for details.
#' @param fun.aggregate Te function to aggregate multiple data before dcasting.
#' @param sep Character vector of length 1, used to separate parts of the
#'   variable names (`_` by default).
#' @param ... Arguments passed to the aggregating function.
#' @param margins Note implemented yet.
#' @param subset Should the dcasting be done on a subset of the data?
#' @param fill Value with which to fill missing cells.
#' @param drop `FALSE` should dcast include all missing combinations?
#' @param value.var The name of the column to use as value variable. If not
#'   provided it is "guessed" the `guess()` internal function is used to build
#'   a good default name.
#' @param verbose Not used yet.
#'
#' @returns A keyed data.trame is returned with the dcasted data.
#' @export
#'
#' @seealso [data.table::dcast()], [melt()]
#'
#' @examples
#' # Adapted from first example of ?dcast.data.table
#' ChickWeight = as.data.trame(ChickWeight)
#' ChickWeight <- set_names(ChickWeight, tolower(names(ChickWeight)))
#' dtrm <- melt(ChickWeight, id.vars = 2:4)
#' dcast(dtrm, time ~ variable, fun.aggregate = mean)
dcast <- function(data, formula, fun.aggregate = NULL, ..., margins = NULL,
    subset = NULL, fill = NULL, value.var = guess(data)) {
  UseMethod("dcast", data)
}

#' @export
#' @rdname dcast
dcast.data.trame <- function(data, formula, fun.aggregate = NULL,
  sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE,
  value.var = guess(data), verbose = getOption("datatable.verbose")) {
  let_data.trame_to_data.table(data)
  on.exit(let_data.table_to_data.trame(data))
  res <- do.call(.dcast.data.table, list(data, formula = formula,
    fun.aggregate = fun.aggregate,..., margins = margins, subset = subset,
    fill = fill, value.var = value.var), envir = parent.frame())
  let_data.table_to_data.trame(res)
  res
}

.dcast.data.table <- data.table::dcast.data.table
