% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{methods}
\alias{methods}
\alias{head.data.trame}
\alias{tail.data.trame}
\alias{names<-.data.trame}
\alias{set_names}
\alias{let_names}
\alias{row.names<-.data.trame}
\alias{set_row_names}
\alias{let_row_names}
\alias{edit.data.trame}
\alias{cbind.data.trame}
\alias{rbind.data.trame}
\title{Various methods implemented for data.trame objects.}
\usage{
\method{head}{data.trame}(x, n = 6L, ...)

\method{tail}{data.trame}(x, n = 6L, ...)

\method{names}{data.trame}(x) <- value

set_names()

let_names(x, value)

\method{row.names}{data.trame}(x) <- value

set_row_names(x, value)

let_row_names(x, value)

\method{edit}{data.trame}(name, ...)

\method{cbind}{data.trame}(
  x,
  ...,
  keep.rownames = FALSE,
  check.names = FALSE,
  key = NULL,
  stringsAsFactors = FALSE
)

\method{rbind}{data.trame}(
  x,
  ...,
  use.names = TRUE,
  fill = FALSE,
  idcol = NULL,
  ignore.attr = FALSE
)
}
\arguments{
\item{x}{A data.trame object.}

\item{n}{The number of rows to keep.}

\item{...}{Further parameters (not used yet).}

\item{value}{The value passed to the method.}

\item{name}{The name of the data.trame to edit.}

\item{keep.rownames}{If \code{TRUE}, the row names are kept as a column}

\item{check.names}{If \code{TRUE}, the names of the columns are checked}

\item{key}{The key to set on the resulting data.trame. If \code{NULL}, no key is set.}

\item{stringsAsFactors}{If \code{TRUE}, character columns are converted to factors.}

\item{use.names}{If \code{TRUE}, the names of the columns are matched. If \code{FALSE},
match is done by position. if \code{"check"}, warn if names do not match.}

\item{fill}{If \code{TRUE}, fill missing columns with \code{NA}. By default \code{FALSE}.}

\item{idcol}{Create a column with ids showing where the data come from. With
\code{TRUE}, the column is named \code{id}. With \code{idcol = "col_name"}, it has that
name.}

\item{ignore.attr}{If \code{TRUE}, ignore attributes when binding.}
}
\value{
\code{head()} and \code{tail()} return truncated data.trame objects (n first
or last rows). \code{names(dtrm) <- value} and \code{set_names()} both set names
(colnames). \code{let_names()} change names by reference.
\code{row.names(dtrm) <- value} and \code{set_row_names()} both set the
row names of a data.trame. However, only the second one keeps the selfref
pointer integrity. \code{let_row_names()} is a faster version, but it changes the
row names by reference. With all \code{let_xxx()} functions, you need to take
extra care to avoid unexpected side effects, see examples. \code{cbind()} combines
data.trames by columns, \code{rbind()} combines them by rows.
}
\description{
These methods handle data.trame objects correctly, so that they remain
internally consistent with data.tables.
}
\examples{
dtrm <- data.trame(
  a = 1:10,
  b = letters[1:10],
  c = factor(LETTERS[1:10]), .key = c('a', 'b')
)
head(dtrm)
tail(dtrm, n = 3L)
cbind(dtrm, dtrm)
rbind(dtrm, dtrm)
dtrm2 <- set_row_names(dtrm, paste("row", letters[1:10]))
dtrm2
dtrm # Not changed
# Take care with let_xxx() functions: it propagates changes to other
# data.trames if you did not used copy()!
dtrm2 <- dtrm
dtrm3 <- data.table::copy(dtrm)
let_row_names(dtrm2, paste("row", letters[11:20]))
dtrm2 # OK
dtrm # Also changed!
dtrm3 # Not changed, because created using copy()
}
