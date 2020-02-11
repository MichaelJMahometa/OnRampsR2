#' Linear Fit and plot of data with a predicted value
#'
#' Show where a given predicted value will occur, give an input of x.
#'
#' @param simpleformula a designation for the model formula in \code{y ~ x} notation. Must be in formula notation--see details.
#' @param xval a value of x that you wish to see a predicted value for.
#' @param xlab optional. Text for x-axis title label.
#' @param ylab optional. Text for y-axis title label.
#' @param newxlim optional. Custom limits for the x axis.
#' @param newylim optional. Custom limits for the y axis.
#' @param showpred logical. Should the predicted point value be plotted on the graph?
#'
#' @seealso
#' \code{\link{linFit}}
#' \code{\link{expFitPred}}
#' \code{\link{logisticFitPred}}
#'
#' @examples
#' data(wolf)
#' wolf <- wolf %>%
#'   mutate(Time = Year - min(Year))
#' linFit(Number ~ Time, data=wolf)
#' linFitPred(Number ~ Time, data=wolf, xval=6)
#'
#' @export
linFitPred <- function (simpleformula, data, xval, xlab = NULL, ylab = NULL, newxlim = NULL, newylim = NULL, showpred = TRUE)
{
  if(is.formula(simpleformula) == FALSE){
    stop("Must use formula y ~ x notation. See Details.")
  }
  nlhs <- length(lazyeval::f_lhs(simpleformula))
  nrhs <- length(lazyeval::f_rhs(simpleformula))
  if(nlhs > 1){
    stop("More than one outcome in formula.\n Please use 'y ~ x' notation.")
  }
  if(nrhs > 1){
    stop("More than one predictor in formula. \n Please use 'y ~ x' notation.")
  }
  mf <- model.frame(simpleformula, data)

  y1 <- as.numeric(mf[[1]])
  x1 <- as.numeric(mf[[2]])

  lin_model <- summary(lm(y1 ~ x1))
  b0 <- lin_model$coef[1]
  b1 <- lin_model$coef[2]
  gx <- seq(min(x1, xval, na.rm = TRUE), max(c(x1, xval, na.rm = TRUE)),
            length = 100)
  gfit <- b0 + (b1 * gx)
  predy <- b0 + (b1 * xval)

  if (!is.null(newxlim)) {
    thisxlim <- newxlim
    gx <- seq(thisxlim[1], thisxlim[2], length = 100)
    gfit <- b0 + (b1 * gx)
  }
  else {
    thisxlim <- c(min(gx), max(c(gx, xval), na.rm = TRUE))
  }
  if (!is.null(newylim)) {
    thisylim <- newylim
  }
  else {
    thisylim <- c(min(c(y1, gfit)), max(c(y1, gfit), na.rm = TRUE))
  }
  pdf <- data.frame(gfit, gx)
  df_pred <- data.frame(predy, xval)

  if(is.null(xlab)){
    xlab <- names(mf)[2]
  }
  if(is.null(ylab)){
    ylab <- names(mf)[1]
  }
  g <- ggformula::gf_point(simpleformula, data=data) %>%
    ggformula::gf_line(gfit ~ gx, data=pdf) %>%
    ggformula::gf_labs(title="Simple Linear Fit: Predicted",
                       x=xlab,
                       y=ylab) %>%
    ggformula::gf_lims(y = thisylim, x=thisxlim) %>%
    ggformula::gf_theme(theme_bw())
  # plot(x1, y1, main = "Linear", pch = 16, xlim = thisxlim, ylim = thisylim, xlab = xlab, ylab = ylab)
  # lines(gx, gfit)
  if (showpred == TRUE) {
    df_pred <- data.frame(predy, xval)
    g <- g %>%
      ggformula::gf_point(predy ~ xval, df_pred, color="red") %>%
      ggformula::gf_labs(subtitle = paste("Predicted value at ", xval,": ", round(predy, 3), sep = ""))
    print(g)
  }
  else {
    g <- g %>%
      ggformula::gf_labs(subtitle = paste0("Predicted Values from ", thisxlim[1],
                                           " to ", thisxlim[2]))
    print(g)
  }
  pred.value <- round(predy, 3)
  return(invisible(list(pred = pred.value, graph = g)))
}
