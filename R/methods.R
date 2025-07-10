# data.table methods that we can keep:
# all.equal, anyDuplicated, as.list, as.matrix, colnames<-, dim, dimnames,
# dimnames<-, droplevels, duplicated, is.na, kronecker, names, names<-,
# na.omit, unique, subset, transform, within

# TODO: transpose() is not a generic function, but it returns a data.table
# -> I need my own version (transpose_() ?), what about t() ?
# TODO: cbind2(), rbind2() and S4 support

# head/tail already return a data.trame
#head.data.trame <- function(x, n = 6L, ...) {
#  setattr(NextMethod("head"), 'class',
#    c('data.trame', 'data.table', 'data.frame'))
#}

#tail.data.trame <- function(x, n = 6L, ...) {
#  setattr(NextMethod("tail"), 'class',
#    c('data.trame', 'data.table', 'data.frame'))
#}

#split?

# We don't need Rd for these methods because they are the same as for data.table
# only that they return a data.trame instead of a data.table
# This one was tested manually: DF3 <- edit(DF)
#' @export
#' @noRd
edit.data.trame <- function(name, ...) {
  setattr(NextMethod("edit"), 'class',
    c('data.trame', 'data.table', 'data.frame'))
}

# NextMethod() cannot be used for cbind() or rbind()
# -> this is a workaround!
#' @export
#' @noRd
.cbind <- function(..., keep.rownames = FALSE, check.names = FALSE,
    key = NULL, stringsAsFactors = FALSE) {
  UseMethod(".cbind")
}

#' @export
#' @noRd
.cbind.data.table <- getS3method("cbind", "data.table")

#' @export
#' @noRd
cbind.data.trame <- function(..., keep.rownames = FALSE, check.names = FALSE,
   key = NULL, stringsAsFactors = FALSE) {
  res <- do.call(.cbind, list(..., keep.rownames = keep.rownames,
    check.names = check.names, key = key, stringsAsFactors = stringsAsFactors),
    envir = parent.frame())
  setattr(res, 'class', c('data.trame', 'data.table', 'data.frame'))
}

#' @export
#' @noRd
.rbind <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL,
    ignore.attr = FALSE) {
  UseMethod(".rbind")
}

#' @export
#' @noRd
.rbind.data.table <- getS3method("rbind", "data.table")

#' @export
#' @noRd
rbind.data.trame <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL,
    ignore.attr = FALSE) {
  res <- do.call(.rbind, list(..., use.names = use.names, fill = fill,
    idcol = idcol, ignore.attr = ignore.attr), envir = parent.frame())
  setattr(res, 'class', c('data.trame', 'data.table', 'data.frame'))
}


# TODO:
#dcast.data.trame
#melt.data.trame
#merge.data.trame

#rollup.data.trame
#cube.data.trame
#groupinsets.data.trame

