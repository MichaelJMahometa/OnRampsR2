#' Logistic Growth Fit and plot of data with a predicted value
#'
#' Show where a given predicted value will occur, give an input of x (logistic growth models).
#'
#' @inheritParams linFitPred
#'
#' @seealso
#' \code{\link{logisticFit}}
#' \code{\link{linFitPred}}
#' \code{\link{expFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' logisticFit(wolf$Time, wolf$Number)
#' logisticFitPred(wolf$Time, wolf$Number, 6)
#'
#' @export
logisticFitPred <- function (x, y, xval, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), newxlim=NULL, newylim=NULL, showpred = TRUE)
{
  y1 <- as.numeric(y)
  x1 <- as.numeric(x)
  y1[y1==0] <- 0.000000001
  log.ss <- nls(y1 ~ SSlogis(x1, phi1, phi2, phi3))
  C <- summary(log.ss)$coef[1]
  a <- (exp((summary(log.ss)$coef[2]) * (1/summary(log.ss)$coef[3])))
  b <- exp((1/summary(log.ss)$coef[3]))
  gx <- seq(min(x1, na.rm=TRUE), max(c(x1, xval)), length=100)
  gfit <- C / (1 + (a*b^-gx))
  if(!is.null(newxlim)){
    thisxlim <- newxlim
  } else {
    thisxlim <- c(min(gx), max(c(gx, xval), na.rm = TRUE))
  }
  if(!is.null(newylim)){
    thisylim <- newylim
  } else {
    thisylim <- c(min(gfit), max(c(y1, gfit), na.rm = TRUE))
  }
  plot(x1, y1, main = "Logistic Function", pch = 16, xlim=thisxlim, ylim=thisylim, xlab=xlab, ylab=ylab)
  lines(gx, gfit)
  predy <- C / (1 + a*b^-xval)
  if(showpred == TRUE){
    points(xval, predy, pch = 16, col = "red")
    mtext(paste("Predicted value: ", round(predy, 3), sep = ""), 3)
    pred.value <- round(predy,3)
    return(invisible(pred.value))
  } else {
    mtext("Predicted values", 3)
  }
}
