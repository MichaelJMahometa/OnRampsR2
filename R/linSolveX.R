#' Solve for X based on models parameters
#'
#' Given some set of parameters specific to a particular model, solve for x.
#'
#' @param y value of a y-axis point (single point).
#' @param a intercept value of for a \strong{simple linear} model.
#' @param b slope value of for a \strong{simple linear} model.
#'
#' @seealso
#' \code{\link{linFit}}
#' \code{\link{linFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' l <- linFit(wolf$Time, wolf$Number)
#' linSolveX(100, l$Intercept, l$Slope)
#'
#' @keywords internal
#'
#' @export
linSolveX <- function(y,a,b){
  x <- (y-a)/b
  return(x)
}


