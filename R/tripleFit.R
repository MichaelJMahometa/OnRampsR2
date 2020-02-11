#' Fit three models at once
#'
#' Fit a linear, exponential, and logistic growth model to two variables and plot the model.
#'
#' @details This function is one of a group of functions that take a specific input in the form of \code{y ~ x}, following (in-part) the mosaic package formula notation. However, only simple formulas are allowed with no grouping parameter, nor multiple predictor variables.
#'
#' @inheritParams linFit
#'
#' @seealso
#' \code{\link{linFit}}
#' \code{\link{expFit}}
#' \code{\link{logisticFit}}
#'
#' @examples
#' data(wolf)
#' wolf <- wolf %>%
#'   mutate(Time = Year - min(Year))
#' tripleFit(Number ~ Time, data=wolf)
#'
#' @export
tripleFit <- function (simpleformula, data, xlab = NULL, ylab = NULL)
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

  y <- as.numeric(mf[[1]])
  x <- as.numeric(mf[[2]])

  y[y == 0] <- 1e-09

  # plot(x, y, main = "Three fits", pch = 16)
  # abline(lm(y ~ x), lty = 1)
  gx <- data.frame(x = seq(min(x, na.rm = TRUE), max(x), length = 100))
  lin_model <- summary(lm(y ~ x))
  fitdf <- data.frame(gx = gx, ylin = predict(lm(y ~ x), gx))

  lin.r2 <- lin_model$r.squared
  ylog <- log(y)
  lin_model.2 <- summary(lm(ylog ~ x))
  a <- exp(lin_model.2$coef[1])
  b <- exp(lin_model.2$coef[2])
  #gx <- seq(min(x, na.rm = TRUE), max(x), length = 100)
  gfit <- a * (b^gx)
  fitdf <- data.frame(fitdf, yexp = gfit)
  #edf <- data.frame(gfit, gx)

  #lines(gx, gfit, lty = 2)
  exp.r2 <- lin_model.2$r.squared
  log.ss <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
  gx <- seq(min(x, na.rm = TRUE), max(x), length = 100)
  pred <- predict(log.ss, data.frame(x = gx))
  fitdf <- data.frame(fitdf, ylog = as.numeric(pred))
  names(fitdf) <- c("gx", "ylin", "yexp", "ylog")
  #ldf <- data.frame(pred, gx)

  #lines(gx, pred, lty = 3)
  r1 <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- sum(residuals(log.ss)^2)
  log.r2 <- (r1 - r2)/r1

  fitdf_long <- fitdf %>%
    tidyr::gather(key=fit_type, value=yfit, ylin, yexp, ylog)

  if(is.null(xlab)){
    xlab <- names(mf)[2]
  }
  if(is.null(ylab)){
    ylab <- names(mf)[1]
  }

  g <- ggformula::gf_point(simpleformula, data=data) %>%
    ggformula::gf_refine(geom_line(data=fitdf_long, aes(x=gx, y=yfit, linetype=fit_type))) %>%
    ggformula::gf_labs(title="Triple Fit",
                       subtitle = paste0("Linear: ", round(lin.r2, 4), "; ",
                                         "Exponential: ", round(exp.r2, 4), "; ",
                                         "Logistic: ", round(log.r2, 4)),
                       x=xlab,
                       y=ylab) %>%
    ggformula::gf_refine(scale_linetype_manual(name="Fit Type",
                                               labels=c("Linear", "Exponential", "Logistic"),
                                               values = c(1,2,3))) %>%
    ggformula::gf_theme(theme_bw())
  print(g)
  # legend("topleft", legend = c("Linear", "Exponential", "Logistic"),
  #        lty = c(1, 2, 3))
  # mtext(paste("Linear R2: ", round(lin.r2, 3), " | Exponential R2: ",
  #             round(exp.r2, 3), " | Logistic R2: ", round(log.r2, 3),
  #             sep = ""), 3)
  cat(" R-squared values for fits:",
      "\n", "Linear: ", round(lin.r2, 4),
      "\n", "Exponential: ", round(exp.r2, 4),
      "\n", "Logistic: ", round(log.r2, 4))
  fits <- list(Linear = lin.r2, Exponential = exp.r2, Logistic = log.r2)
  return(invisible(list(fits = fits, graph=g)))
}
