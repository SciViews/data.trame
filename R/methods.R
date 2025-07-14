# data.table methods that we can keep:
# all.equal, anyDuplicated, as.list, as.matrix, colnames<-, dim, dimnames,
# dimnames<-, droplevels, duplicated, is.na, kronecker, names, names<-,
# na.omit, unique, subset, transform, within

# TODO: transpose() is not a generic function, but it returns a data.table
# -> I need my own version (transpose_() ?), what about t() ?
# TODO: cbind2(), rbind2() and S4 support

# head/tail, no need to change

#split?

# We don't need Rd for these methods because they are the same as for data.table
# only that they return a data.trame instead of a data.table
# This one was tested manually: DF3 <- edit(DF)
#' @export
#' @noRd
edit.data.trame <- function(name, ...) {
  qDT(NextMethod("edit", name), class = .dtrm_class)
}

# NextMethod() cannot be used for cbind() or rbind()
# -> this is a workaround!
.cbind.data.table <- getS3method("cbind", "data.table")

#' @export
#' @noRd
cbind.data.trame <- function(x, ..., keep.rownames = FALSE, check.names = FALSE,
   key = NULL, stringsAsFactors = FALSE) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  .cbind.data.table(x, ..., keep.rownames = keep.rownames,
    check.names = check.names, key = key, stringsAsFactors = stringsAsFactors)
}

.rbind.data.table <- getS3method("rbind", "data.table")

#' @export
#' @noRd
rbind.data.trame <- function(x, ..., use.names = TRUE, fill = FALSE,
    idcol = NULL, ignore.attr = FALSE) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  .rbind.data.table(x, ..., use.names = use.names,
    fill = fill, idcol = idcol, ignore.attr = ignore.attr)
}

# TODO:
#merge.data.trame
#merge.data.trame <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,
#    all = FALSE, all.x = all, all.y = all, sort = TRUE,
#    suffixes = c(".x", ".y"), no.dups = TRUE,
#    allow.cartesian = getOption("datatable.allow.cartesian"),
#    incomparables = NULL, ...) {
#  setattr(NextMethod("merge"), 'class',
#    c('data.trame', 'data.table', 'data.frame'))
#}
# Error in `[.default`(x, i) : invalid subscript type 'list'
#(dt1 <- data.trame(A = letters[1:10], X = 1:10, key = "A"))
#(dt2 <- data.trame(A = letters[5:14], Y = 1:10, key = "A"))
#merge(dt1, dt2)

#rollup.data.trame does not work, using NextMethod() or this one:
# Example rollup =>
#rollup(DT, j = sum(value), by = by_vars) # default id=FALSE
#Error in eval(jj) : object 'value' not found
#.rollup <- function(x, j, by, .SDcols, id = FALSE, ...) {
#  UseMethod(".rollup")
#}

#.rollup.data.table <- getS3method("rollup", "data.table")

#rollup.data.trame <- function(x, j, by, .SDcols, id = FALSE, ...) {
#  setattr(x, 'class',
#    c('data.trame', 'data.table', 'data.frame'))
#  res <- do.call(.rollup, list(x, j = substitute(j), by = substitute(by),
#    .SDcols = substitute(.SDcols), id = id, ...),
#    envir = parent.frame())
#  setattr(res, 'class', c('data.trame', 'data.table', 'data.frame'))
#}

# Similar problems with cube() and groupinsets()
#cube.data.trame
#groupinsets.data.trame

