#' Solve for X based on models parameters
#'
#' Given some set of parameters specific to a particular model, solve for x.
#'
#' @param y value of a y-axis point.
#' @param a intercept value of for a \strong{exponential} model.
#' @param b growth factor value of for a \strong{exponential} model.
#'
#' @seealso
#' \code{\link{expFit}}
#' \code{\link{expFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' e <- expFit(wolf$Time, wolf$Number)
#' expSolveX(100, e$a, e$b)
#'
#' @keywords internal
#'
#' @export
expSolveX <- function(y,a,b){
  x <- log(y/a)/log(b)
  return(x)
}


