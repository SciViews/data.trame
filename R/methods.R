# data.table methods that we can keep:
# all.equal, anyDuplicated, as.matrix, colnames<-, dim, dimnames,
# dimnames<-, droplevels, duplicated, is.na, kronecker,
# na.omit, unique, subset

# Need to implement as.list() (eliminate sorted and row.names),
# transform (returns a data.frame), within (does not work)
# TODO: transpose() is not a generic function, but it returns a data.table
# -> I need my own version (transpose_() ?), what about t() ?
# TODO: cbind2(), rbind2() and S4 support

#split? rbindlist()


#' Various methods implemented for data.trame objects.
#'
#' These methods handle data.trame objects correctly, so that they remain
#' internally consistent with data.tables.
#'
#' @param x A data.trame object.
#' @param n The number of rows to keep.
#' @param ... Further parameters (not used yet).
#' @param value The value passed to the method.
#'
#' @returns `head()` and `tail()` return truncated data.trame objects (n first
#' or last rows). `names(dtrm) <- value` and `set_names()` both set names
#' (colnames). `let_names()` change names by reference.
#' `row.names(dtrm) <- value` and `set_row_names()` both set the
#' row names of a data.trame. However, only the second one keeps the selfref
#' pointer integrity. `let_row_names()` is a faster version, but it changes the
#' row names by reference. With all `let_xxx()` functions, you need to take
#' extra care to avoid unexpected side effects, see examples. `cbind()` combines
#' data.trames by columns, `rbind()` combines them by rows.
#' @export
#' @name methods
#'
#' @examples
#' dtrm <- data.trame(
#'   a = 1:10,
#'   b = letters[1:10],
#'   c = factor(LETTERS[1:10]), .key = c('a', 'b')
#' )
#' head(dtrm)
#' tail(dtrm, n = 3L)
#' cbind(dtrm, dtrm)
#' rbind(dtrm, dtrm)
#' dtrm2 <- set_row_names(dtrm, paste("row", letters[1:10]))
#' dtrm2
#' dtrm # Not changed
#' # Take care with let_xxx() functions: it propagates changes to other
#' # data.trames if you did not used copy()!
#' dtrm2 <- dtrm
#' dtrm3 <- data.table::copy(dtrm)
#' let_row_names(dtrm2, paste("row", letters[11:20]))
#' dtrm2 # OK
#' dtrm # Also changed!
#' dtrm3 # Not changed, because created using copy()
head.data.trame <- function(x, n = 6L, ...) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  head(x, n = n, ...)
}

#' @rdname methods
#' @export
tail.data.trame <- function(x, n = 6L, ...) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  tail(x, n = n, ...)
}

# TODO: deal with key!
#' @rdname methods
#' @export
`names<-.data.trame` <- function(x, value) {
  # value must be character with no missing values,
  # and of the same length as the number of rows
  # Convert everything into character
  value <- as.character(value)
  if (length(value) != length(x) || anyNA(value))
    stop("Names must be a character vector of the same length as x with no missing values")
  rn <- attr(x, 'row.names')
  x <- qDT(x, keep.attr = TRUE, class = .dtrm_class)
  setattr(x, "row.names", rn)
  # If there are keys, we must also rename them
  key <- attr(x, "sorted")
  if (is.null(key)) {# None, no problems
    setnames(x, value)
  } else { # We have a key, so we must rename it
    conv <- structure(value, names = names(x))
    setattr(x, "sorted", conv[key])
    setnames(x, value)
  }
}

#' @rdname methods
#' @export
set_names <- `names<-`

#' @rdname methods
#' @export
let_names <- function(x, value) {
  value <- as.character(value)
  if (length(value) != length(x) || anyNA(value))
    stop("Names must be a character vector of the same length as x with no missing values")
  # If there are keys, we must also rename them
  key <- attr(x, "sorted")
  if (is.null(key)) {# None, no problems
    setnames(x, value)
  } else { # We have a key, so we must rename it
    conv <- structure(value, names = names(x))
    setattr(x, "sorted", conv[key])
    setnames(x, value)
  }
}

#' @rdname methods
#' @export
`row.names<-.data.trame` <- function(x, value) {
  # value must be either integer or character with no missing values,
  # no duplicates and of the same length as the number of rows
  # Convert everything into character
  value <- as.character(value)
  if (!check_character(value, len = fnrow(x), any.missing = FALSE, unique = TRUE))
    stop("Row names must be a character vector with no missing values and no duplicates")
  x <- qDT(x, keep.attr = TRUE, class = .dtrm_class)
  setattr(x, "row.names", value)
}

#' @rdname methods
#' @export
set_row_names <- `row.names<-`

#' @rdname methods
#' @export
let_row_names <- function(x, value) {
  if (!is.data.frame(x))
    stop("Cannot let row names on a non-data.frame object")
  value <- as.character(value)
  if (!check_character(value, len = fnrow(x), any.missing = FALSE, unique = TRUE))
    stop("Row names must be a character vector with no missing values and no duplicates")
  setattr(x, "row.names", value)
}

# We don't need Rd for these methods because they are the same as for data.table
# only that they return a data.trame instead of a data.table
# This one was tested manually: DF3 <- edit(DF)
#' @rdname methods
#' @export
#' @param name The name of the data.trame to edit.
edit.data.trame <- function(name, ...) {
  qDT(NextMethod("edit", name), class = .dtrm_class)
}

#' @rdname methods
#' @param keep.rownames If `TRUE`, the row names are kept as a column
#' @param check.names If `TRUE`, the names of the columns are checked
#' @param key The key to set on the resulting data.trame. If `NULL`, no key is set.
#' @param stringsAsFactors If `TRUE`, character columns are converted to factors.
#' @export
cbind.data.trame <- function(x, ..., keep.rownames = FALSE, check.names = FALSE,
   key = NULL, stringsAsFactors = FALSE) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  res <- cbind(x, ..., keep.rownames = keep.rownames, check.names = check.names,
    key = key, stringsAsFactors = stringsAsFactors)
  let_data.table_to_data.trame(res)
  res
}

# TODO: deal with rownames!
# TODO: rbindlist()
#' @rdname methods
#' @param use.names If `TRUE`, the names of the columns are matched. If `FALSE`,
#'   match is done by position. if `"check"`, warn if names do not match.
#' @param fill If `TRUE`, fill missing columns with `NA`. By default `FALSE`.
#' @param idcol Create a column with ids showing where the data come from. With
#'   `TRUE`, the column is named `id`. With `idcol = "col_name"`, it has that
#'   name.
#' @param ignore.attr If `TRUE`, ignore attributes when binding.
#' @export
rbind.data.trame <- function(x, ..., use.names = TRUE, fill = FALSE,
    idcol = NULL, ignore.attr = FALSE) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  res <- rbind(x, ..., use.names = use.names, fill = fill, idcol = idcol,
    ignore.attr = ignore.attr)
  let_data.table_to_data.trame(res)
  res
}

# TODO: man page for merge
#' @export
#' @noRd
merge.data.trame <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,
    all = FALSE, all.x = all, all.y = all, sort = TRUE,
    suffixes = c(".x", ".y"), no.dups = TRUE,
    allow.cartesian = getOption("datatable.allow.cartesian"),
    incomparables = NULL, ...) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  if (missing(by)) {
    res <- merge(x, y, by.x = by.x, by.y = by.y, all = all,
      all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes,
      no.dups = no.dups, allow.cartesian = allow.cartesian,
      incomparables = incomparables, ...)
  } else {
    if (!missing(by.x) || !missing(by.y)) {
      warning("Supplied both `by` and `by.x`/`by.y`. `by` argument will be ignored.")
      res <- merge(x, y, by.x = by.x, by.y = by.y, all = all,
        all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes,
        no.dups = no.dups, allow.cartesian = allow.cartesian,
        incomparables = incomparables, ...)
    } else {
      res <- merge(x, y, by = by, all = all,
        all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes,
        no.dups = no.dups, allow.cartesian = allow.cartesian,
        incomparables = incomparables, ...)
    }
  }
  let_data.table_to_data.trame(res)
  res
}

#(dt1 <- data.trame(A = letters[1:10], X = 1:10, key = "A"))
#(dt2 <- data.trame(A = letters[5:14], Y = 1:10, key = "A"))
#merge(dt1, dt2)


#.rollup.data.table <- getS3method("rollup", "data.table")

# TODO: man page for rollup, cube and groupingsets
#' @export
#' @noRd
rollup.data.trame <- function(x, j, by, .SDcols, id = FALSE, ...) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  res <- do.call(rollup, list(x, j = substitute(j), by = substitute(by),
    .SDcols = substitute(.SDcols), id = id, ...),
    envir = parent.frame())
  let_data.table_to_data.trame(res)
  res
}

# Examples from data.table::rollup()
#n = 24L
#set.seed(25)
#dtrm <- data.trame(
#  color  = sample(c("green","yellow","red"), n, TRUE),
#  year   = as.Date(sample(paste0(2011:2015,"-01-01"), n, TRUE)),
#  status = as.factor(sample(c("removed", "active", "inactive", "archived"),
#    n, TRUE)),
#  amount = sample(1:5, n, TRUE),
#  value  = sample(c(3, 3.5, 2.5, 2), n, TRUE)
#)
#
## rollup
#by_vars = c("color", "year", "status")
#rollup(dtrm, j = sum(value), by = by_vars) # default id = FALSE
#rollup(dtrm, j = sum(value), by = by_vars, id = TRUE)

#' @export
#' @noRd
cube.data.trame <- function(x, j, by, .SDcols, id = FALSE, label = NULL, ...)  {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  res <- do.call(cube, list(x, j = substitute(j), by = substitute(by),
    .SDcols = substitute(.SDcols), id = id, label = label, ...),
    envir = parent.frame())
  let_data.table_to_data.trame(res)
  res
}

#cube(dtrm, j = sum(value), by = c("color","year","status"), id = TRUE)

#' @export
#' @noRd
groupingsets.data.trame <- function(x, j, by, sets, .SDcols, id = FALSE, jj,
    label = NULL, ...)  {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  if (missing(jj)) {
    res <- do.call(groupingsets, list(x, j = substitute(j), by = substitute(by),
      sets = substitute(sets), .SDcols = substitute(.SDcols), id = id,
      label = label, ...), envir = parent.frame())
  } else {
    res <- do.call(groupingsets, list(x, j = substitute(j), by = substitute(by),
      sets = substitute(sets), .SDcols = substitute(.SDcols), id = id, jj = jj,
      label = label, ...), envir = parent.frame())
  }
  let_data.table_to_data.trame(res)
  res
}

#groupingsets(dtrm, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"),
#. sets = list("color", c("year","status"), character()), id = TRUE)
