#' Solve for X based on models parameters
#'
#' Given some set of parameters specific to a particular model, solve for x.
#'
#' @param y value of a y-axis point.
#' @param C value of for the Carrying Capactiy parameter in a \strong{logistic growth} model.
#' @param a value of for the "a" parameter in a \strong{logistic growth} model.
#' @param b value of for the "b" parameter in a \strong{logistic growth} model.
#'
#' @seealso
#' \code{\link{logisticFit}}
#' \code{\link{logisticFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' l <- logisticFit(wolf$Time, wolf$Number)
#' logisticSolveX(100, l$C, l$a, l$b)
#'
#' @keywords internal
#'
#' @export
logisticSolveX <- function(y,C,a,b){
  x <- -1 * (log(((C/y)-1)/a)/log(b))
  return(x)
}
