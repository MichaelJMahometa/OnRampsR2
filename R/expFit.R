#' Exponential Fit and plot of data
#'
#' Fit a exponential model to two variables, return basic output about the model, and plot the model.
#'
#' @inheritParams linFit
#'
#' @seealso
#' \code{\link{linFit}}
#' \code{\link{logisticFit}}
#' \code{\link{tripleFit}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' expFit(wolf$Time, wolf$Number)
#' expFit(wolf$Time, wolf$Number, xlab="Years since 1995", ylab="Number of Wolves")

#' @export
expFit <- function(x,y,xlab=deparse(substitute(x)), ylab=deparse(substitute(y))){
  y1 <- as.numeric(y)
  x1 <- as.numeric(x)
  y1[y1==0] <- 0.000000001
  ylog <- log(y1)
  lin_model <- summary(lm(ylog~x1))
  lin_int <- lin_model$coef[1]
  lin_slope <- lin_model$coef[2]
  a <- exp(lin_int)
  b <- exp(lin_slope)
  r2 <- lin_model$r.squared
  lotx <- seq(min(x1), max(x1), length=100)
  fity <- a*(b^lotx)
  plot(x,y,main="Exponential", pch=16, xlab=xlab, ylab=ylab)
  lines(lotx,fity)
  exp.out <- list(a=a, b=b, r_sq=r2)
  cat(' a = ',round(a,5),'\n','b = ',round(b,5), '\n','R-squared = ',round(r2,5))
  return(invisible(exp.out))
}
