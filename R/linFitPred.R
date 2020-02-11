#' Linear Fit and plot of data with a predicted value
#'
#' Show where a given predicted value will occur, give an input of x.
#'
#' @param x a designation for a variable on the x-axis. Can be a vector or object.
#' @param y a designation for a variable on the y-axis. Can be a vector or object.
#' @param xval a value of x that you wish to see a predicted value for.
#' @param xlab optional. Text for x-axis title label.
#' @param ylab optional. Text for y-axis title label.
#' @param newxlim optional. Custom limits for the x axis.
#' @param newylim optional. Custom limits for the y axis.
#' @param showpred logical. Should the predicted point value be plotted on the graph?
#'
#'#' @seealso
#' \code{\link{linFit}}
#' \code{\link{expFitPred}}
#' \code{\link{logisticFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf$Time <- wolf$Year - min(wolf$Year)
#' linFit(wolf$Time, wolf$Number)
#' linFitPred(wolf$Time, wolf$Number, 6)
#'
#' @export
linFitPred <- function (x, y, xval, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), newxlim=NULL, newylim=NULL, showpred = TRUE)
{
  y1 <- as.numeric(y)
  x1 <- as.numeric(x)
  lin_model <- summary(lm(y1 ~ x1))
  b0 <- lin_model$coef[1]
  b1 <- lin_model$coef[2]
  gx <- seq(min(x1, xval, na.rm=TRUE), max(c(x1, xval, na.rm=TRUE)), length=100)
  gfit <- b0 + (b1*gx)
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
  plot(x1, y1, main = "Linear", pch = 16, xlim=thisxlim, ylim=thisylim, xlab=xlab, ylab=ylab)
  lines(gx, gfit)
  predy <- b0 + (b1*xval)
  if(showpred == TRUE){
    points(xval, predy, pch = 16, col = "red")
    mtext(paste("Predicted value: ", round(predy, 3), sep = ""), 3)
    pred.value <- round(predy,2)
    return(invisible(pred.value))
  } else {
    mtext("Predicted values", 3)
  }
}
