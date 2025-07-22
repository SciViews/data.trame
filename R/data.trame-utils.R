# Note: no test is made here to check if the object is a valid data.table or
# data.trame! You have to do it yourself.
.dtrm_class <- c('data.trame', 'data.frame')

# Quickly set the class of a data.trame object (warning: change byref!)
# Must be a valid data.table object first (no test here!)
.let_data.trame_class <- function(x) {
  setattr(x, 'class', .dtrm_class)
}

#' Switch back and forth quickly between data.trame and data.table by reference
#'
#' These functions just change the class of the object by reference (data.trame
#' and data.table objects are, internally, identical except for their class).
#' These functions are intended only for programmers! There is no check that the
#' object is correct before the change.
#'
#' @param x A valid data.trame or data.table object
#'
#' @returns A data.table or data.trame object.
#' @export
#'
#' @examples
#' dtrm <- data.trame(a = 1:3, b = letters[1:3], c = factor(LETTERS[1:3]))
#' class(dtrm)
#' let_data.trame_to_data.table(dtrm)
#' class(dtrm)
#' let_data.table_to_data.trame(dtrm)
#' class(dtrm)
#'
#' # Whenever you need to use a data.table method on a data.trame in a function,
#' # you can do something like this:
#' test_fun <- function(x, i, expr) {
#'   force(i) # Make sure i is evaluated before changing the class of x
#'   # value will be evaluated with x being a data.table here!
#'   let_data.trame_to_data.table(x)
#'   on.exit(let_data.table_to_data.trame(x))
#'   print(class(x)) # Internally, it is now a data.table
#'   expr
#' }
#' test_fun(dtrm, 1:2, class(dtrm))
#' class(dtrm) # Back to data.trame
let_data.table_to_data.trame <- function(x) {
  setv(class(x), 'data.table', 'data.trame')
}

#' @rdname let_data.table_to_data.trame
#' @export
let_data.trame_to_data.table <- function(x) {
  setv(class(x), 'data.trame', 'data.table')
}

# The shallow() function is not exported by data.table, so we copy it here
.shallow <- data.table:::shallow
