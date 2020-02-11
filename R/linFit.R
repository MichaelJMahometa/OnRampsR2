#' Linear Fit and plot of data
#'
#' Fit a linear model to two variables, return basic output about the model, and plot the model.
#'
#' @details This function is one of a group of functions that take a specific input in the form of \code{y ~ x}, following (in-part) the mosaic package formula notation. However, only simple formulas are allowed with no grouping parameter, nor multiple predictor variables.
#'
#' @param simpleformula a designation for the model formula in \code{y ~ x} notation. Must be in formula notation--see details.
#' @param data a data frame in which to evaluate formulas.
#' @param xlab optional. Text for x-axis title label for produced graph.
#' @param ylab optional. Text for y-axis title label for produced graph.
#'
#' @seealso
#' \code{\link{expFit}}
#' \code{\link{logisticFit}}
#' \code{\link{tripleFit}}
#'
#' @examples
#' data(wolf)
#' linFit(Number ~ Year, data=wolf)
#' linFit(Number ~ Year, data=wolf, xlab="Year", ylab="Number of Wolves")
#'
#' @export
linFit <- function (simpleformula, data, xlab = NULL, ylab = NULL)
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
  r2 <- lin_model$r.squared
  if(is.null(xlab)){
    xlab <- names(mf)[2]
  }
  if(is.null(ylab)){
    ylab <- names(mf)[1]
  }
  g <- ggformula::gf_point(simpleformula, data=data) %>%
    ggformula::gf_lm() %>%
    ggformula::gf_labs(title="Simple Linear Fit",
                       x=xlab,
                       y=ylab) %>%
    ggformula::gf_theme(theme_bw())
  print(g)
  # plot(x1, y1, main = "Linear", pch = 16, xlab = xlab, ylab = ylab)
  # abline(lm(y1 ~ x1))
  lin.out <- list(Intercept = b0, Slope = b1, r_sq = r2)
  cat(" Intercept = ", round(b0, 5),
      "\n", "Slope = ", round(b1, 5),
      "\n", "R-squared = ", round(r2, 5))
  return(invisible(list(terms=lin.out, graph=g)))
}
