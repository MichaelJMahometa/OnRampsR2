#' Logistic Growth Fit and plot of data
#'
#' Fit a logistic growth model to two variables, return basic output about the model, and plot the model.
#'
#' @details This function is one of a group of functions that take a specific input in the form of \code{y ~ x}, following (in-part) the mosaic package formula notation. However, only simple formulas are allowed with no grouping parameter, nor multiple predictor variables.
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
#' wolf <- wolf %>%
#'   mutate(Time = Year - min(Year))
#' logisticFit(Number ~ Time, data=wolf)
#' logisticFit(Number ~ Time, data=wolf, xlab="Years since 1995", ylab="Number of Wolves")
#'
#' @export
logisticFit <- function (simpleformula, data, xlab = NULL, ylab = NULL)
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
  log.ss <- nls(y1 ~ SSlogis(x1, phi1, phi2, phi3))
  C <- summary(log.ss)$coef[1]
  a <- (exp((summary(log.ss)$coef[2]) * (1/summary(log.ss)$coef[3])))
  b <- exp((1/summary(log.ss)$coef[3]))
  lotx <- seq(min(x1), max(x1), length = 100)
  fity <- predict(log.ss, data.frame(x1 = lotx))
  pdf <- data.frame(fity, lotx)

  if(is.null(xlab)){
    xlab <- names(mf)[2]
  }
  if(is.null(ylab)){
    ylab <- names(mf)[1]
  }
  g <- ggformula::gf_point(simpleformula, data=data) %>%
    ggformula::gf_line(fity ~ lotx, data=pdf) %>%
    ggformula::gf_labs(title="Simple Logistic Growth Fit",
                       x=xlab,
                       y=ylab) %>%
    ggformula::gf_theme(theme_bw())
  print(g)

  # plot(x, y, main = "Logistic Function", pch = 16, xlab = xlab, ylab = ylab)
  # lines(lotx, pred)
  r1 <- sum((y1 - mean(y1, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- sum(residuals(log.ss)^2)
  r_sq <- (r1 - r2)/r1
  log.out <- list(C = C, a = a, b = b, r_sq = r_sq)
  cat("Logistic Fit", "\n", "C = ", round(C, 5), "\n", "a = ",
      round(a, 5), "\n", "b = ", round(b, 5), "\n", "R-squared = ",
      round(r_sq, 5))
  return(invisible(list(terms = log.out, graph=g)))
}
