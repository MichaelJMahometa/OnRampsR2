#' Exponential Fit and plot of data
#'
#' Fit a exponential model to two variables, return basic output about the model, and plot the model.
#'
#' @details This function is one of a group of functions that take a specific input in the form of \code{y ~ x}, following (in-part) the mosaic package formula notation. However, only simple formulas are allowed with no grouping parameter, nor multiple predictor variables.
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
#' wolf <- wolf %>%
#'   mutate(Time = Year - min(Year))
#' expFit(Number ~ Time, data=wolf)
#' expFit(Number ~ Time, data=wolf, xlab="Years since 1995", ylab="Number of Wolves")

#' @export
expFit <- function (simpleformula, data, xlab = NULL, ylab = NULL)
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
  r2 <- lin_model$r.squared
  lotx <- seq(min(x1), max(x1), length = 100)
  fity <- a * (b^lotx)
  pdf <- data.frame(fity, lotx)

  if(is.null(xlab)){
    xlab <- names(mf)[2]
  }
  if(is.null(ylab)){
    ylab <- names(mf)[1]
  }
  g <- ggformula::gf_point(simpleformula, data=data) %>%
    ggformula::gf_line(fity ~ lotx, data=pdf) %>%
    ggformula::gf_labs(title="Simple Exponential Fit",
                       x=xlab,
                       y=ylab) %>%
    ggformula::gf_theme(theme_bw())
  print(g)
  # plot(x, y, main = "Exponential", pch = 16, xlab = xlab, ylab = ylab)
  # lines(lotx, fity)
  exp.out <- list(a = a, b = b, r_sq = r2)
  cat(" a = ", round(a, 5), "\n", "b = ", round(b, 5), "\n",
      "R-squared = ", round(r2, 5))
  return(invisible(list(terms = exp.out, graph=g)))
}
