# TODO: replace as_tibble() in the as.xxx() functions by qTBL(), because
# as_tibble() does not strip away the .internal.selfref attribute of data.table
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
#'   'data.trame'/'data.frame' object, thus subclassing 'data.frame'.
#'   `is.data.trame()` returns `TRUE` if the
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
  res <- as_tibble(list(...), .rows = .rows, .namerepair = .name_repair)
  if (!missing(.key)) {
    res <- qDT(res)
    setkeyv(res, .key)
    let_data.table_to_data.trame(res)
  } else {
    res <- qDT(res, class = c(.dtrm_class)) # Note: c() does a copy!
  }
  # qDT() only overallocate 100 positions, use setalloccol() now
  #setalloccol(res)
  # Another option:
  #setDT(res, keep.rownames = FALSE, key = .key, check.names = FALSE)
  #let_data.table_to_data.trame(res)
  res
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
  check_dots_empty()
  res <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
    rownames = .rownames)
  rnames <- row.names(res)

  if (!missing(.key)) {
    res <- qDT(res)
    setkeyv(res, .key)
    let_data.table_to_data.trame(res)
  } else {
    res <- qDT(res, class = .dtrm_class)
  }
  if (is.character(rnames))
    setattr(res, "row.names", rnames)
  res
}

#' @rdname data.trame
#' @export
as.data.trame.list <- function(x, .key = NULL, .rows = NULL,
  .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty()
  res <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
    rownames = .rownames)
  rnames <- row.names(res)
  if (!missing(.key)) {
    res <- qDT(res)
    setkeyv(res, .key)
    let_data.table_to_data.trame(res)
  } else {
    res <- qDT(res, class = .dtrm_class)
  }
  if (is.character(rnames))
    setattr(res, "row.names", rnames)
  res
}

#' @rdname data.trame
#' @export
as.data.trame.data.frame <- function(x, .key = NULL, .rows = NULL,
    .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty()
  if (!missing(.rows) || !missing(.rownames) || !missing(.name_repair))
    x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
      rownames = .rownames)
  rnames <- row.names(x)
  if (!missing(.key)) {
    res <- qDT(x)
    setkeyv(res, .key)
    let_data.table_to_data.trame(res)
  } else {
    res <- qDT(x, class = .dtrm_class)
  }
  if (is.character(rnames))
    setattr(res, "row.names", rnames)
  res
}

#' @rdname data.trame
#' @export
as.data.trame.data.trame <- function(x, .key = NULL, .rows = NULL,
  .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty()
  if (missing(.key))
    .key <- key(x)
  if (!missing(.rows) || !missing(.rownames) || !missing(.name_repair)) {
    x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
      rownames = .rownames)
    rnames <- row.names(x)
    if (!is.null(.key)) {
      x <- qDT(x)
      setkeyv(x, .key)
      let_data.table_to_data.trame(x)
    } else {
      x <- qDT(x, class = .dtrm_class)
    }
    if (is.character(rnames))
      setattr(x, "row.names", rnames)
  } else if (!is.null(.key)) {
    let_data.trame_to_data.table(x)
    on.exit(let_data.table_to_data.trame(x))
    setkeyv(x, .key)
  }
  x
}

#' @rdname data.trame
#' @export
as.data.trame.data.table <- function(x, .key = NULL, .rows = NULL,
  .rownames = NA, .name_repair = c("check_unique", "unique", "universal",
    "minimal"), ...) {
  check_dots_empty()
  if (missing(.key))
    .key <- key(x)
  if (!missing(.rows) || !missing(.rownames) || !missing(.name_repair))
    x <- as_tibble(x, .rows = .rows, .name_repair = .name_repair,
      rownames = .rownames)
  rnames <- row.names(x)
  if (!is.null(.key)) {
    res <- qDT(x)
    setkeyv(res, .key)
    let_data.table_to_data.trame(res)
  } else {
    res <- qDT(x, class = .dtrm_class)
  }
  if (is.character(rnames))
    setattr(res, "row.names", rnames)
  res
}

#' @rdname data.trame
#' @param keep.rownames For compatibility with the generic, but not used here
#' @export
as.data.table.data.trame <- function(x, keep.rownames = FALSE, ...) {
  check_dots_empty()
  qDT(x, row.names.col = keep.rownames, keep.attr = TRUE)
}

#' @rdname data.trame
#' @export
#' @param row.names A character vector of row names, or `NULL` (default).
#' @param optional If `TRUE`, the row names are not checked for uniqueness.
#'   If `FALSE`, the row names are corrected with [make.names()].
as.data.frame.data.trame <- function(x, row.names = NULL, optional = TRUE, ...,
    keep.attr = FALSE) {
  check_dots_empty()
  res <- qDF(x, row.names.col = NULL, keep.attr = keep.attr)
  if (!isTRUE(optional))
    names(res) <- make.names(names(res), unique = TRUE)
  res
}

#' @rdname data.trame
#' @param keep.rownames For compatibility with the generic, but not used here
#' @param rownames `NULL` remove row names, `NA` (default) leaves them, or a
#' single string to create a column with that name.
#' @param keep.attr If `TRUE`, the attributes of the data frame are kept.
#' @export
as_tibble.data.trame <- function(x, ..., .rows = NULL, .name_repair =
    c("check_unique", "unique", "universal", "minimal"), rownames = NULL,
    keep.attr = FALSE) {
  check_dots_empty()
  # Translate rownames into row.names.col parlance (NULL, means FALSE)
  if (is.null(rownames))
    rownames <- FALSE
  res <- qTBL(x, row.names.col = rownames, keep.attr = keep.attr)
  if (is.na(rownames))
    setattr(res, "row.names", row.names(x))
  if (!missing(.rows) || !missing(.name_repair)) {
    res <- as_tibble(res, .rows = .rows, .name_repair = .name_repair,
      rownames = NA)
  }
  res
}

#' @rdname data.trame
#' @param x An object.
#' @export
is.data.trame <- function(x) inherits(x, 'data.trame')
