# data.table methods that we can keep:
# all.equal, anyDuplicated, as.list, as.matrix, colnames<-, dim, dimnames,
# dimnames<-, droplevels, duplicated, is.na, kronecker, names, names<-,
# na.omit

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

# TODO:
#cbind.data.trame
#cbind2.data.trame
#rbind.data.trame
#rbind2.data.trame

#edit.data.trame

#dcast.data.trame
#melt.data.trame
#merge.data.trame
#rollup.data.trame
#cube.data.trame

#groupinsets.data.trame

#subset.data.trame

#transform.data.trame

#unique.data.trame

#within.data.trame
