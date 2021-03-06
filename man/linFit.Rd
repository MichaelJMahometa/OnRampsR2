% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linFit.R
\name{linFit}
\alias{linFit}
\title{Linear Fit and plot of data}
\usage{
linFit(simpleformula, data, xlab = NULL, ylab = NULL)
}
\arguments{
\item{simpleformula}{a designation for the model formula in \code{y ~ x} notation. Must be in formula notation--see details.}

\item{data}{a data frame in which to evaluate formulas.}

\item{xlab}{optional. Text for x-axis title label for produced graph.}

\item{ylab}{optional. Text for y-axis title label for produced graph.}
}
\description{
Fit a linear model to two variables, return basic output about the model, and plot the model.
}
\details{
This function is one of a group of functions that take a specific input in the form of \code{y ~ x}, following (in-part) the mosaic package formula notation. However, only simple formulas are allowed with no grouping parameter, nor multiple predictor variables.
}
\examples{
data(wolf)
linFit(Number ~ Year, data=wolf)
linFit(Number ~ Year, data=wolf, xlab="Year", ylab="Number of Wolves")

}
\seealso{
\code{\link{expFit}}
\code{\link{logisticFit}}
\code{\link{tripleFit}}
}
