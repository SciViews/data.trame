#' Printing data.trames
#'
#' A data.trame prints almost like a tibble.
#'
#' @param x A data.trame object.
#' @param width The width of the text output. If `NULL`, the default,
#'   `getOption("width")` is used.
#' @param ... Additional arguments passed to `format()`.
#' @param n The number of rows to print. If `NULL`, a default number is used.
#' @param max_extra_cols The maximum number of extra columns to print
#'   abbreviated. If `NULL` by default, a reasonable default value is used.
#' @param max_footer_lines Maximum number of lines in the footer. If `NULL`, a
#'   reasonable default value is used.
#'
#' @export
#'
#' @examples
#' dtrm <- data.trame(
#'   a = -1:3,
#'   b = letters[1:5],
#'   c = factor(LETTERS[1:5]),
#'   d = c(TRUE, FALSE, TRUE, NA, TRUE), .key = c('a', 'b'), .rows = 5
#' )
#' dtrm
#' str(dtrm)
print.data.trame <- function(x, width = NULL, ..., n = NULL,
  max_extra_cols = NULL, max_footer_lines = NULL) {
  y <- as_tibble(x, .name_repair = "minimal")
  class(y) <- unique(c("datatrame", class(y)))
  print(y, width = width, ..., n = n, max_extra_cols = max_extra_cols,
    max_footer_lines = max_footer_lines)
  invisible(x)
}

#' @rdname print.data.trame
#' @export
format.data.trame <- function(x, width = NULL, ..., n = NULL,
  max_extra_cols = NULL, max_footer_lines = NULL) {
  y <- as_tibble(x)
  class(y) <- unique(c("datatrame", class(y)))
  format(y, width = width, ..., n = n, max_extra_cols = max_extra_cols,
    max_footer_lines = max_footer_lines)
}

#' @rdname print.data.trame
#' @export
obj_sum.datatrame <- function(x) {
  abbr <- "dtrm"
  out <- paste(abbr, size_sum(x))
  structure(out, short = abbr)
}

#' @rdname print.data.trame
#' @export
obj_sum.data.trame <- obj_sum.datatrame

#' @rdname print.data.trame
#' @export
tbl_sum.datatrame <- function(x, ...) {
  res <-  c(`A data.trame` = size_sum(x))

  key <- key(x)
  if (!is.null(key)) {
    res <- c(res, Key = paste(key, collapse = ", "))
  #} else {
  #  message("key is NULL")
  }

  lang <- attr(comment(x), "lang")
  if (!is.null(lang))
    res <- c(res, Language = lang)

  res
}

#' @rdname print.data.trame
#' @export
tbl_sum.data.trame <- tbl_sum.datatrame

#' @rdname print.data.trame
#' @export
tbl_nrow.data.trame <- function(x, ...) {
  nrow(x)
}

#' @rdname print.data.trame
#' @param object A data.trame.
#' @param indent.str The string used for indentation.
#' @param nest.lev The current nesting level, used for recursive printing.
#' @export
str.data.trame <- function(object, ..., indent.str = " ", nest.lev = 0) {
  if (nest.lev != 0L)
    cat(" ")
  cat(obj_sum(object), " (S3: ", paste0(class(object),
    collapse = "/"), ")", "\n", sep = "")
  str(as.list(object), no.list = TRUE, ..., nest.lev = nest.lev +
      1L, indent.str = indent.str)
}
