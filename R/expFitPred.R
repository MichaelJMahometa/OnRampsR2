#' Exponential Fit and plot of data with a predicted value
#'
#' Show where a given predicted value will occur, give an input of x (exponential models).
#'
#' @inheritParams linFitPred
#'
#' @seealso
#' \code{\link{expFit}}
#' \code{\link{linFitPred}}
#' \code{\link{logisticFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' expFit(wolf$Time, wolf$Number)
#' expFitPred(wolf$Time, wolf$Number, 6)
#'
#' @export
expFitPred <- function (x, y, xval, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), newxlim=NULL, newylim=NULL, showpred = TRUE)
{
  y1 <- as.numeric(y)
  x1 <- as.numeric(x)
  y1[y1==0] <- 0.000000001
  ylog <- log(y1)
  lin_model <- summary(lm(ylog ~ x1))
  lin_int <- lin_model$coef[1]
  lin_slope <- lin_model$coef[2]
  a <- exp(lin_int)
  b <- exp(lin_slope)
  gx <- seq(min(x1, na.rm=TRUE), max(c(x1, xval)), length=100)
  gfit <- a * (b^gx)
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
  plot(x1, y1, main = "Exponential", pch = 16, xlim=thisxlim, ylim=thisylim, xlab=xlab, ylab=ylab)
  lines(gx, gfit)
  predy <- a * (b^xval)
  if(showpred == TRUE){
    points(xval, predy, pch = 16, col = "red")
    mtext(paste("Predicted value: ", round(predy, 3), sep = ""), 3)
    pred.value <- round(predy,3)
    return(invisible(pred.value))
  } else {
    mtext("Predicted values", 3)
  }
}
