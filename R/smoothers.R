#-------------------------------------------------------------------------------
# Smoothers
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Whittaker 1st order smoother
#'
#' @description `r lifecycle::badge("stable")`
#' Apply the whittaker 1st order smoother to a matrix.
#'
#' @param x matrix. The data to smooth by row.
#' @param lambda numeric. The smoother paramater.
#'
#' @return A matrix with the same dimensions as \code{x}.
#' @export
#' @importFrom ptw whit1
#'
#' @examples
#' \dontrun{
#' set.seed(12029)
#' n <- 20
#' x <- matrix(1:n + rnorm(n), nrow = 1, ncol = n)
#' x[ , 5] <- NA
#' y <- wt1(x, lambda = 2)
#' plot(x[1, ])
#' lines(y[1, ], col = "blue")
#' }
wt1 <- function(x, lambda = 50) {
  z <- x
  for (i in 1:nrow(x)) {
    y <- x[i, ]
    k <- is.na(y)
    w <- rep(1, length(y))
    w[is.na(y)] <- 0
    y[is.na(y)] <- 0
    z[i, ] <- ptw::whit1(y, lambda, w)
  }
  z
}
