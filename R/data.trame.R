#' Build, coerce and test for 'data.trame' objects
#'
#' @param ... A set of name-value pairs that constitute the 'data.trame'.
#' @param .key A character vector with the name of the columns to use as key
#'   (sorting the data)
#' @param .rows The number of rows in the final 'data.trame'. Useful to create a
#'   0-column object, or for additional check.
#' @param .name_repair Treatment of problematic column names: could be
#'   `"check_unique"` (default, no name repair but check they are unique),
#'   `"unique"` (make sure names are unique), `"universal"` (make sure names are
#'   unique and syntactically correct), `"minimal"` (no check or name repair
#'   except for existence), or a function for custom name repair, e.g.,
#'   `.name.repair = make.names` for base R style.
#'
#' @returns A 'data.trame' object, which is indeed a
#'   'data.trame'/'data.table'/'data.frame' object, thus subclassing both
#'   'data.table' and 'data.frame'. `is.data.trame()` returns `TRUE` if the
#'   object is a 'data.trame', and `FALSE` otherwise.
#' @export
#'
#' @examples
#' dtrm <- data.trame(
#'   a = 1:3,
#'   b = letters[1:3],
#'   c = factor(LETTERS[1:3]), .key = c('a', 'b'), .rows = 3
#' )
#' is.data.trame(dtrm)
data.trame <- function(..., .key = NULL, .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  dtrm <- tibble(..., .rows = .rows, .name_repair = .name_repair)
  #setDT(dtrm, keep.rownames = TRUE, key = .key)
  setattr(dtrm, 'class', c('data.trame', 'data.table', 'data.frame'))
  setalloccol(dtrm)
  if (!is.null(.key))
    setkeyv(dtrm, .key)
  dtrm
}

#' @rdname data.trame
#' @param .rownames The name of the column that holds the row names of the
#'   original object, if any. If `NA` (default), row names are left intact. If
#'   `NULL` row names are removed.
#' @export
as.data.trame <- function(x, .key = NULL, .rows = NULL, .rownames = NA,
    .name_repair = c("check_unique", "unique", "universal", "minimal"), ...) {
  UseMethod("as.data.trame")
}

#' @rdname data.trame
#' @export
as.data.trame.default <- function(x, .key = NULL, .rows = NULL,
    .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty0()
  x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
    rownames = .rownames)
  #setDT(x, keep.rownames = TRUE, key = .key)
  setattr(x, 'class', c('data.trame', 'data.table', 'data.frame'))
  setalloccol(x)
  if (!is.null(.key))
    setkeyv(x, .key)
  x
}

#' @rdname data.trame
#' @export
as.data.trame.data.frame <- function(x, .key = NULL, .rows = NULL,
    .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty0()
  if (!missing(.rows) || !missing(.rownames) || !missing(.name_repair))
    x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
      rownames = .rownames)
  setattr(x, 'class', c('data.trame', 'data.table', 'data.frame'))
  setalloccol(x)
  if (!is.null(.key))
    setkeyv(x, .key)
  x
}

#' @rdname data.trame
#' @export
as.data.trame.data.table <- function(x, .key = NULL, .rows = NULL,
    .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty0()
  if (!missing(.rows) || !missing(.rownames) || !missing(.name_repair)) {
    x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
      rownames = .rownames)
  }
  setattr(x, 'class', c('data.trame', 'data.table', 'data.frame'))
  setalloccol(x)
  if (!is.null(.key))
    setkeyv(x, .key)
  x
}

#' @rdname data.trame
#' @export
as.data.trame.data.trame <- function(x, .key = NULL, .rows = NULL,
  .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty0()
  if (!missing(.rows) || !missing(.rownames) || !missing(.name_repair)) {
    x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
      rownames = .rownames)
    setattr(x, 'class', c('data.trame', 'data.table', 'data.frame'))
    setalloccol(x)
  }
  if (!is.null(.key))
    setkeyv(x, .key)
  x
}

#as.tibble <- svMisc::aka(tibble::as_tibble) # Should be nice to have
#as_tibble.data.trame == as_tibble.data.frame
#as.data.frame.data.trame == as.data.frame.data.table
#as.data.table.data.trame == as.data.table.data.table

#' @rdname data.trame
#' @param x An object.
#' @export
is.data.trame <- function(x) inherits(x, 'data.trame')
