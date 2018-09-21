#' Logistic Growth Fit and plot of data
#'
#' Fit a logistic growth model to two variables, return basic output about the model, and plot the model.
#'
#' @inheritParams linFit
#'
#' @seealso
#' \code{\link{linFit}}
#' \code{\link{expFit}}
#' \code{\link{tripleFit}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' logisticFit(wolf$Time, wolf$Number)
#' logisticFit(wolf$Time, wolf$Number, xlab="Years Since 1995", ylab="Number of Wolves")
#'
#' @export
logisticFit <- function(x, y, xlab=deparse(substitute(x)), ylab=deparse(substitute(y))){
  y1 <- as.numeric(y)
  x1 <- as.numeric(x)
  y1[y1==0] <- 0.000000001
  log.ss <- nls(y1 ~ SSlogis(x1, phi1, phi2, phi3))
  C <- summary(log.ss)$coef[1]
  a <- (exp((summary(log.ss)$coef[2]) * (1/summary(log.ss)$coef[3])))
  b <- exp((1 / summary(log.ss)$coef[3]))
  lotx <- seq(min(x1), max(x1), length=100)
  plot(x,y,main = "Logistic Function", pch=16, xlab=xlab, ylab=ylab)
  pred <- predict(log.ss, data.frame(x1=lotx))
  lines(lotx, pred)

  r1 <- sum((y1 - mean(y1, na.rm=TRUE))^2, na.rm=TRUE)
  r2 <- sum(residuals(log.ss)^2)

  r_sq <- (r1 - r2) / r1
  log.out <- list(C=C, a=a, b=b, r_sq=r_sq)
  cat('Logistic Fit','\n','C = ',round(C,5),'\n','a = ',round(a,5),'\n','b = ',round(b,5), '\n','R-squared = ',round(r_sq,5))
  return(invisible(log.out))
}
