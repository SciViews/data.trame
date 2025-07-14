# Subsetting is done in SE way, except if formulas are provided, then, it is the
# data.table syntax that is used with the rhs expression in the formulas

#' Subsetting data.trames
#'
#' Subsetting data.trames uses a syntax similar to tibble, or formulas for `i`,
#' `j`, and possibly `by` or `keyby` to use the data.table syntax instead.
#'
#' @param x A data.trame object.
#' @param i Selection of rows by indices, negative indices, logical or a formula
#' @param j Selection of columns by indices, negative indices, logical, names or
#'   a formula (both i and j must be formulas simultaneously). If `:=` is used
#'   in the formula to create one or more new variables by reference, the
#'   expression must be placed between `{}` to avoid operators precedence
#'   issues, or better: `:=` could be just replaced by `~`.
#' @param by Grouping columns (must be a formula and `j` must be also provided as
#'   a formula)
#' @param keyby Either `TRUE`/`FALSE` if `by` is provided, or a formula (and `j`
#'   must also be provided as a formula)
#' @param with Logical, whether to evaluate `j` in the data.trame if `TRUE` or
#'   in the calling environment if `FALSE` (default is `TRUE`).  `with = FALSE`
#'   is similar to tibble subsetting and it is forced when `i` or `j` are not
#'   formulas.
#' @param drop Coerce to a vector if the returned data.trame only has one column
#' @param ... Further arguments passed to the underlying `data.table` subsetting
#'
#' @returns A data.trame object, or a vector if `drop = TRUE` and the result has
#'   only one column.
#' @export
#' @name subsetting
#'
#' @examples
#' dtrm <- data.trame(
#'   a = 1:3,
#'   b = letters[1:3],
#'   c = factor(LETTERS[1:3])
#' )
#' # Subsetting rows, the tibble-way
#' dtrm[1:2, ]
#' dtrm[-1, ]
#' dtrm[c(TRUE, FALSE, TRUE), ]
#' # On the contrary to data.table, providing only one arg, means subsetting
#' # columns (like for data.frame or tibble)
#' dtrm[c(TRUE, FALSE, TRUE)]
#' dtrm[dtrm$a > 1, ] # Must fully qualify the column name
#' # Subsetting the data.table way, with formulas: no fully qualification needed
#' dtrm[~a > 1, ]
#'
#' # Subsetting the columns, the tibble way
#' dtrm[, 1:2]
#' dtrm[, -1]
#' dtrm[, c(TRUE, FALSE, TRUE)]
#' dtrm[, c("a", "b")]
#' # You must set drop = TRUE explicitly to return a vector
#' dtrm[, 2] # Still a data.trame, like tibble, but unlike the data.frame method
#' dtrm[, 2, drop = TRUE] # Now a vector
#' # The selection is referentially transparent, i.e., you can do:
#' sel <- c("c", "b")
#' dtrm[, sel]
#' # Subsetting the columns, the data.table way, with formulas
#' dtrm[~1:2, ~.(b)]
#' dtrm[~1:2, ~b] # If not enclosed in .(), returns a vector instead
#' # Precautions are needed here because it is NOT referentially transparent:
#' dtrm[, ~..sel] # In data.table language, this is how you access `sel`
#'
#' # Extended data.table syntax using i, j, by, or keyby with formulas
#' # Warning: due to precedence of operators, you must use braces here!
#' dtrm[, ~{d := paste0(b, c)}] # Changed in place (by reference!)
#' # Another form that does not need braces, but is less readable:
#' dtrm[, ~`:=`(e, paste0(b, a))]
#' # or equivalently:
#' dtrm[, ~let(e = paste0(b, a))]
#' # In this case, it is much better to just replace `:=` by `~`, but internally
#' # it uses set(). It is faster, but much more limited and cannot use by or
#' # or keyby:
#' dtrm[, f ~ paste0(c, a)]
#' # One can also use standard evaluation in that case using with = FALSE
#' dtrm[, f ~ paste0(dtrm$c, dtrm$a), with = FALSE]
#' #
#' # Take care when you provide only one argument:
#' # If it is a formula, the data.table syntax is used (select rows)
#' # otherwise, the data.frame syntax applies, and columns are selected!
#' dtrm[1:2] # All rows and 2 first columns
#' dtrm[~1:2] # All columns and 2 first rows!
#'
#' # For $, on the contrary to data.frame/data.table, but like tibble,
#' # no partial match is allowed (returns NULL with a warning)
#' dtrm$count <- dtrm$c
#' names(dtrm)
#' dtrm$count #OK
#' #dtrm$co # Not OK, no partial match allowed
`[.data.trame` <- function(x, i, j, by, keyby, with = TRUE, drop = FALSE, ...) {
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))

  if (missing(j)) {
    # by/keyby can only be provided when j is a formula
    if (!missing(by) || !missing(keyby))
      stop("by and keyby can only be provided when j is a formula")
    if (missing(i)) # Both i and j missing: just return x
       return(x)
    if (inherits(i, "formula")) {
      if (!is.null(f_lhs(i)))
        stop("the formula in i cannot have a left-hand side")
      i <- f_rhs(i)
      res <- do.call(`.[.data.table`, list(x, i, drop = drop, ...),
        envir = parent.frame()) # with applies only on j
    } else {# i is not a formula, use the data.frame syntax:
      # columns selection if not empty j, or row selection
      ij_args <- nargs() - ...length() - !missing(drop) - !missing(with) - 1L
      if (ij_args > 1) {# Row selection
        res <- `.[.data.frame`(x, i, drop = drop)
      } else {# Column selection (like for data.frame)
        res <- `.[.data.frame`(x, j = i, drop = drop)
      }
    }

  } else {# j provided
    if (!inherits(j, "formula")) {# SE syntax
      if (!missing(by) || !missing(keyby))
        stop("by and keyby can only be provided when j is a formula")
      if (missing(i)) {
        res <- `.[.data.frame`(x, j = j, drop = drop)
      } else {# both i and j provided
        if (inherits(i, "formula"))
          stop("both i and j must be formulas simultaneously")
        res <- `.[.data.frame`(x, i, j, drop = drop)
      }

    } else {# j is a formula, use the data.table syntax
      # In case we have x ~ expr, we transform into x := expr
      j_lhs <- f_lhs(j)
      if (!is.null(j_lhs)) {# We use the faster let_() instead of [.data.table
        if (!missing(by) || !missing(keyby))
          stop("by and keyby cannot be provided when j is a formula with a lhs")
        if (missing(i)) {
          i <- NULL # Equivalent
        } else if (inherits(i, "formula")) {
          i <- f_rhs(i)
        }
        # Since let_() uses standard evaluation, but we are in a formula, we want
        # NSE, with the variables of x available, so... wrap the expr in with()
        # but not if the user explicitly specifies with = FALSE
        j_rhs <- f_rhs(j)
        if ((!missing(with) && isFALSE(with)) ||
            is.null(j_rhs)) {# No with construct
          expr <- j_rhs
        } else{# Wrap the expression inside a with() construct
          expr <- quote(with(df, j_rhs))
          expr[[2]] <- substitute(x)
          expr[[3]] <- j_rhs
        }
        # j can be x, "x", 1, or c(x, y), c("x", "y"), c(1, 3)
        # deal with all these forms:
        # x -> class is "name", need to apply as.character()
        # "x" -> class is "character" -> no need to change it
        # 1 -> class is "numeric" -> no need to change it
        # c(x, y) -> class is "call" -> check function is "c", eliminate first
        # item, convert to character vector, and then to numeric if needed
        if (is.numeric(j_lhs)) {
          j <- as.integer(j_lhs)
        } else if (is.name(j_lhs) || is.character(j_lhs)) {
          j <- as.character(j_lhs)
        } else if (is.call(j_lhs) && as.character(j_lhs[[1]]) == "c") {
          j <- as.character(j_lhs[-1]) # Eliminate first item
          if (is.numeric(j_lhs[[2]])) # Possibly convert to integer
            j <- as.integer(j)
        } else {
          stop("j lhs must be like x, \"x\", 1, c(x, y), c(\"x\", \"y\") or c(1, 3)")
        }
        return(do.call(let_, list(substitute(x), i = i, j = j, value = expr),
          envir = parent.frame()))
        # Note: these two line do the job without relying to let_()!
        #j[[1]] <- as.name(":=")
        #attributes(j) <- NULL # Eliminate class and .Environment -> call object
      } else {
        j <- f_rhs(j)
      }
      if (!missing(by)) {
        if (!inherits(by, "formula"))
          stop("by must be a formula")
        if (!is.null(f_lhs(by)))
          stop("the formula in by cannot have a left-hand side")
        by <- f_rhs(by)
        if (missing(keyby)) {
          if (missing(i)) {
            res <- do.call(`.[.data.table`, list(substitute(x), j = j, by = by,
              with = with, drop = FALSE, ...), envir = parent.frame())
          } else {
            if (!inherits(i, "formula"))
              stop("both i and j must be formulas simultaneously")
            if (!is.null(f_lhs(i)))
              stop("the formula in i cannot have a left-hand side")
            i <- f_rhs(i)
            res <- do.call(`.[.data.table`, list(substitute(x), i, j, by,
              with = with, drop = FALSE, ...), envir = parent.frame())
          }
        } else if (!is.logical(keyby)) {
          stop("keyby must be TRUE or FALSE when by is provided")
        } else {
          if (missing(i)) {
            res <- do.call(`.[.data.table`, list(substitute(x), j = j, by = by,
              keyby = keyby, with = with, drop = FALSE, ...),
              envir = parent.frame())
          } else {
            if (!inherits(i, "formula"))
              stop("both i and j must be formulas simultaneously")
            if (!is.null(f_lhs(i)))
              stop("the formula in i cannot have a left-hand side")
            i <- f_rhs(i)
            res <- do.call(`.[.data.table`, list(substitute(x), i, j, by, keyby,
              with = with, drop = FALSE, ...), envir = parent.frame())
          }
        }

      } else {# by is missing
        if (missing(keyby)) {
          if (missing(i)) {
            res <- do.call(`.[.data.table`, list(substitute(x), j = j,
              with = with, drop = FALSE, ...), envir = parent.frame())
          } else {
            if (inherits(i, "formula")) {
              if (!is.null(f_lhs(i)))
                stop("the formula in i cannot have a left-hand side")
              i <- f_rhs(i)
            }
            res <- do.call(`.[.data.table`, list(substitute(x), i, j,
              with = with, drop = FALSE, ...), envir = parent.frame())
          }
        } else {# keyby is present
          if (!inherits(keyby, "formula"))
            stop("keyby must be a formula")
          if (!is.null(f_lhs(keyby)))
            stop("the formula in keyby cannot have a left-hand side")
          keyby <- f_rhs(keyby)
          if (missing(i)) {
            res <- do.call(`.[.data.table`, list(substitute(x), j = j,
              keyby = keyby, with = with, drop = FALSE, ...),
              envir = parent.frame())
          } else {
            if (inherits(i, "formula")) {
              if (!is.null(f_lhs(i)))
                stop("the formula in i cannot have a left-hand side")
              i <- f_rhs(i)
            }
            res <- do.call(`.[.data.table`, list(substitute(x), i, j,
              keyby = keyby, with = with, drop = FALSE, ...),
              envir = parent.frame())
          }
        }
      }
    }
  }
  # Deal with drop = TRUE if a data.frame of only one column is returned
  if (is.data.frame(res) && isTRUE(drop) && ncol(res) == 1) {
    res[[1]]
  } else {
    res
  }
}

#' @rdname subsetting
#' @param name The name of the column to extract.
#' @export
`$.data.trame` <- function(x, name) {
  out <- .subset2(x, name)
  if (is.null(out))
    warning("Unknown or uninitialised column: ", name, ".")
  out
}

# TODO: should we be strict: only 1 or nrow(x) len for i?
# TODO: should we rebuild key (and eliminate if if column removed)? To be tested!
#' @rdname subsetting
#' @param byref Logical, whether to use by reference or not (`FALSE` by default).
#' @param value The value to insert as subassignment in a data.trame object.
#' @export
set_ <- function(x, i, j, value, byref = FALSE) {
  if (missing(i))
    i <- NULL

  if (isTRUE(byref)) {
    i <- switch(class(i)[1],
      matrix = {
        if (!missing(j))
          stop("When i is a matrix in dtrm[i] <- value syntax, it doesn't make sense to provide j")
        x = `[<-.data.frame`(x, i, value = value)
        return(setalloccol(x))
      },
      logical = seq_row(x)[i],
      numeric = as.integer(i),
      character = structure(seq_row(x), names = rownames(x))[i],
      i)

    # j can be column names (character) or numbers (integer)
    if (missing(j)) {# j cannot be missing in set()
      j <- seq_along(x) # Faster than collapse::seq_row()
    } else if (is.double(j)) {
      j <- as.integer(j)
    }

    force(value)

    # No need to switch, since set accepts also data.frame, except if new columns are created,
    # but it takes too long to test => switch all the time
    let_data.trame_to_data.table(x)
    on.exit(let_data.table_to_data.trame(x))
    do.call(set, list(substitute(x), i = i, j = j,
      value = value), envir = parent.frame())

  } else {# byref = FALSE
    NextMethod("[<-", x) # Use the data.frame method instead
  }
}

#' @rdname subsetting
#' @export
let_ <- function(x, i = NULL, j = seq_along(x), value) {
  i <- switch(class(i)[1],
    matrix = {
      if (!missing(j))
        stop("When i is a matrix in dtrm[i] <- value syntax, it doesn't make sense to provide j")
      x = `[<-.data.frame`(x, i, value = value)
      return(setalloccol(x))
    },
    logical = seq_row(x)[i],
    numeric = as.integer(i),
    character = structure(seq_row(x), names = rownames(x))[i],
    i)

  # j can be column names (character) or numbers (integer)
  if (is.double(j))
    j <- as.integer(j)

  force(value)

  # No need to switch, since set accepts also data.frame, except if new columns are created,
  # but it takes too long to test => switch all the time
  let_data.trame_to_data.table(x)
  on.exit(let_data.table_to_data.trame(x))
  do.call(set, list(substitute(x), i = i, j = j,
    value = value), envir = parent.frame())
}

#`.[` <- function(x, i, j, by, keyby, with = TRUE, nomatch = NA, mult = "all",
#    roll = FALSE, rollends = if (roll == "nearest") c(TRUE, TRUE) else
#    if (roll >= 0) c(FALSE, TRUE) else c(TRUE, FALSE), which = FALSE, .SDcols,
#    verbose = getOption("datatable.verbose"),
#    allow.cartesian = getOption("datatable.allow.cartesian"), drop = FALSE,
#    on = NULL, env = NULL,
#    showProgress = getOption("datatable.showProgress", interactive())) {
#  UseMethod(".[")
#}

`.[.data.table` <- getS3method("[", "data.table")

`.[.data.frame` <- function(x, i, j = seq_along(x), by, keyby, with = TRUE,
    nomatch = NA, mult = "all", roll = FALSE, rollends = if (roll == "nearest")
    c(TRUE, TRUE) else if (roll >= 0) c(FALSE, TRUE) else c(TRUE, FALSE),
    which = FALSE, .SDcols, verbose = getOption("datatable.verbose"),
    allow.cartesian = getOption("datatable.allow.cartesian"),
    drop = if (missing(i)) TRUE else ncol(x) == 1, on = NULL, env = NULL,
    showProgress = getOption("datatable.showProgress", interactive())) {
  #if (missing(i) && missing(j))
  #  return(x) # No subsetting, return the object as is
  #xclass <- class(x)
  #x <- as.data.frame(x)
  #if (missing(i)) {
  #  res <- x[, j, drop = drop]
  #} else if (is.null(j)) {
  #  res <- x[i, ]
  #} else if (missing(j)) {
  #  res <- x[i] # It selects on columns, while data.table selects on rows
  #} else {
  #  res <- x[i, j, drop = drop]
  #}
  #if (is.data.frame(res))
  #  res <- as.data.trame(res)
  #res
  if (missing(i)) {
    if (missing(j))
      return(x)
    res <- ss(x, j = j)
  } else {# i provided
    # In case i is character, try matchin on rownames
    if (is.character(i))
      i <- structure(seq_row(x), names = rownames(x))[i]
    res <- ss(x, i, j)
  }
  if (isTRUE(drop) && ncol(res) == 1) {
    res[[1]]
  } else {
    res
  }
}
