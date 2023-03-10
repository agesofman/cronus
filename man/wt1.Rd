% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoothers.R
\name{wt1}
\alias{wt1}
\title{Whittaker 1st order smoother}
\usage{
wt1(x, lambda = 50)
}
\arguments{
\item{x}{matrix. The data to smooth by row.}

\item{lambda}{numeric. The smoother parameter.}
}
\value{
A matrix with the same dimensions as \code{x}.
}
\description{
Apply the whittaker 1st order smoother to a matrix.
}
\examples{
\dontrun{
set.seed(12029)
n <- 20
x <- matrix(1:n + rnorm(n), nrow = 1, ncol = n)
x[ , 5] <- NA
y <- wt1(x, lambda = 2)
plot(x[1, ])
lines(y[1, ], col = "blue")
}
}
