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
#' wolf <- wolf %>%
#'   mutate(Time = Year - min(Year))
#' expFit(Number ~ Time, data=wolf)
#' expFitPred(Number ~ Time, data=wolf, xval=6)
#'
#' @export
expFitPred <- function (simpleformula, data, xval, xlab = NULL, ylab = NULL,
                         newxlim = NULL, newylim = NULL, showpred = TRUE)
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

  y1[y1 == 0] <- 1e-09
  ylog <- log(y1)
  lin_model <- summary(lm(ylog ~ x1))
  lin_int <- lin_model$coef[1]
  lin_slope <- lin_model$coef[2]
  a <- exp(lin_int)
  b <- exp(lin_slope)
  gx <- seq(min(x1, na.rm = TRUE), max(c(x1, xval)), length = 100)
  gfit <- a * (b^gx)
  predy <- a * (b^xval)

  if (!is.null(newxlim)) {
    thisxlim <- newxlim
    gx <- seq(thisxlim[1], thisxlim[2], length = 100)
    gfit <- a * (b^gx)
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
    ggformula::gf_labs(title="Simple Exponential Fit: Predicted",
                       x=xlab,
                       y=ylab) %>%
    ggformula::gf_lims(y = thisylim, x=thisxlim) %>%
    ggformula::gf_theme(theme_bw())
  # print(g)
  # plot(x1, y1, main = "Exponential", pch = 16, xlim = thisxlim,
  #      ylim = thisylim, xlab = xlab, ylab = ylab)
  # lines(gx, gfit)
  if (showpred == TRUE) {
    g <- g %>%
      ggformula::gf_point(predy ~ xval, df_pred, color="red") %>%
      ggformula::gf_labs(subtitle = paste("Predicted value at ", xval,": ", round(predy, 3), sep = ""))
    print(g)
    # points(xval, predy, pch = 16, col = "red")
    # mtext(paste("Predicted value: ", round(predy, 3), sep = ""),
    #       3)
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
