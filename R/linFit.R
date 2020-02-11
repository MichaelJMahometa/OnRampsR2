#' Linear Fit and plot of data
#'
#' Fit a linear model to two variables, return basic output about the model, and plot the model.
#'
#' @param x a designation for a variable on the x-axis. Can be a vector or object.
#' @param y a designation for a variable on the y-axis. Can be a vector or object.
#' @param xlab optional. Text for x-axis title label.
#' @param ylab optional. Text for y-axis title label.
#'
#'#' @seealso
#' \code{\link{expFit}}
#' \code{\link{logisticFit}}
#' \code{\link{tripleFit}}
#'
#' @examples
#' data(wolf)
#' linFit(wolf$Year, wolf$Number)
#' linFit(wolf$Year, wolf$Number, xlab="Year", ylab="Number of Wolves")
#'
#' @export
linFit <- function(x,y, xlab=deparse(substitute(x)), ylab=deparse(substitute(y))){
  y1 <- as.numeric(y)
  x1 <- as.numeric(x)
  lin_model <- summary(lm(y1~x1))
  b0 <- lin_model$coef[1]
  b1 <- lin_model$coef[2]
  r2 <- lin_model$r.squared
  plot(x1,y1,main="Linear", pch=16, xlab=xlab, ylab=ylab)
  abline(lm(y1~x1))
  lin.out <- list(Intercept=b0, Slope=b1, r_sq=r2)
  cat(' Intercept = ',round(b0,5),'\n','Slope = ',round(b1,5), '\n','R-squared = ',round(r2,5))
  return(invisible(lin.out))
}
