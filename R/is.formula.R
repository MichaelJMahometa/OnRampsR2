#' Is the object a formula
#'
#' Checks if the object given is a formula notation.
#'
#' @param x and object.
#'
#' @keywords internal
#'
#' @export
is.formula <- function(x){
  inherits(x,"formula")
}
