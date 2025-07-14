# t.data.trame = t.data.table = t.data.frame
# Math.data.trame = Math.data.table = Math.data.frame
# Summary.data.trame = Summary.data.table = Summary.data.frame

#' @noRd
#' @export
Ops.data.trame <- function(e1, e2 = NULL) {
  res <- NextMethod()
  if (is.data.frame(res)) {
    res <- as.data.trame(res)
  } else if (is.matrix(res)) {
    colnames(res) <- copy(colnames(res))
  }
  res
}
