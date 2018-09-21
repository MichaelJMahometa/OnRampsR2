#' Fit three models at once
#'
#' Fit a linear, exponential, and logistic growth model to two variables and plot the model.
#'
#' @param x a designation for a variable on the x-axis. Must be a vector.
#' @param y a designation for a variable on the y-axis. Must be a vector.
#'
#' @seealso
#' \code{\link{linFit}}
#' \code{\link{expFit}}
#' \code{\link{logisticFit}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' tripleFit(wolf$Time, wolf$Number)
#'
#' @export
tripleFit <- function(x, y){
  y <- as.numeric(y)
  x <- as.numeric(x)
  y[y==0] <- 0.000000001

  plot(x,y, main = "Three fits", pch=16)
  #Add linear
  #abline(lm(y~x), col="green")
  abline(lm(y~x), lty=1)
  lin_model <- summary(lm(y~x))
  lin.r2 <- lin_model$r.squared

  #Add Exponential
  ylog <- log(y)
  lin_model.2 <- summary(lm(ylog~x))
  a <- exp(lin_model.2$coef[1])
  b <- exp(lin_model.2$coef[2])
  gx <- seq(min(x, na.rm=TRUE), max(x), length=100)
  gfit <- a * (b^gx)
  #lines(x,fity, col="blue")
  lines(gx,gfit, lty=2)
  exp.r2 <- lin_model.2$r.squared

  #Add Logistic
  #Log Self-starting...
  log.ss <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
  gx <- seq(min(x, na.rm=TRUE), max(x), length=100)
  pred <- predict(log.ss, data.frame(x=gx))
  #lines(x, pred, col="red")
  lines(gx, pred, lty=3)
  r1 <- sum((y - mean(y, na.rm=TRUE))^2, na.rm=TRUE)
  r2 <- sum(residuals(log.ss)^2)
  log.r2 <- (r1 - r2) / r1

  legend("topleft", legend=c("Linear", "Exponential","Logistic"),
         #col=c("green", "blue", "red"),
         lty=c(1,2,3))

  mtext(paste("Linear R2: ", round(lin.r2,3), " | Exponential R2: ", round(exp.r2, 3), " | Logistic R2: ", round(log.r2, 3), sep=""), 3)

  fits <- list(linear = lin.r2, exponential = exp.r2, logistic = log.r2)
  return(invisible(fits))
}
