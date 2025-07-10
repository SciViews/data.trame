#' @noRd
Ops.data.trame <- function(e1, e2 = NULL) {
  res = NextMethod()
  if (is.data.table(res))
    setattr(res, 'class', c('data.trame', 'data.table', 'data.frame'))
  res
}
