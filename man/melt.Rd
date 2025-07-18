% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/melt.R
\name{melt}
\alias{melt}
\alias{melt.data.trame}
\title{Fast melt for data.trame}
\usage{
melt(data, ..., na.rm = FALSE, value.name = "value")

\method{melt}{data.trame}(
  data,
  id.vars,
  measure.vars,
  variable.name = "variable",
  value.name = "value",
  ...,
  na.rm = FALSE,
  variable.factor = TRUE,
  value.factor = FALSE,
  verbose = getOption("datatable.verbose")
)
}
\arguments{
\item{data}{A data.trame object.}

\item{...}{Arguments passed to other methods.}

\item{na.rm}{Should \code{NA} values be removed? Default is \code{FALSE}.}

\item{value.name}{Name for the molten data values column(s).}

\item{id.vars}{Vector of \code{id} variables.}

\item{measure.vars}{Measure variables for melting. Can be missing.}

\item{variable.name}{Name (default \code{variable}) of output column containing
the information about melted columns.}

\item{variable.factor}{If \code{TRUE}, the variable column is converted to a
factor, otherwise, it is a character column.}

\item{value.factor}{If \code{TRUE}, the value column is converted to a factor,
else, it is left unchanged.}

\item{verbose}{\code{TRUE} turns on status and information messages.}
}
\value{
An unkeyed data.trame containing the molten data.
}
\description{
\verb{melt() is reshaping wide-to-long, notb unlike }tidyr::pivot_longer()` for
data.trame objects. See \code{\link[data.table:melt.data.table]{data.table::melt()}} for explanations.
}
\examples{
# Adapted from first example of ?melt.data.table
set.seed(45)
library(data.trame)
dtrm <- data.trame(
  i_1 = c(1:5, NA),
  n_1 = c(NA, 6, 7, 8, 9, 10),
  f_1 = factor(sample(c(letters[1:3], NA), 6L, TRUE)),
  f_2 = ordered(c("z", "a", "x", "c", "x", "x")),
  c_1 = sample(c(letters[1:3], NA), 6L, TRUE),
  c_2 = sample(c(LETTERS[1:2], NA), 6L, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin = "2013-09-01"),
  d_2 = as.Date(6:1, origin = "2012-01-01")
)
# add a couple of list cols
dtrm$l_1 <- dtrm[, ~list(c = list(rep(i_1, sample(5, 1L)))), by = ~i_1]$c
dtrm$l_2 <- dtrm[, ~list(c = list(rep(c_1, sample(5, 1L)))), by = ~i_1]$c

# id.vars, measure.vars as character/integer/numeric vectors
melt(dtrm, id.vars = 1:2, measure.vars = "f_1")
}
\seealso{
\code{\link[data.table:melt.data.table]{data.table::melt()}}, \code{\link[=dcast]{dcast()}}
}
