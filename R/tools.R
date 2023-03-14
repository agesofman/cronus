#-------------------------------------------------------------------------------
# Tool functions
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Combine elements
#'
#' @description Create all unique element combinations.
#'
#' @param ... arguments
#'
#' @return A `list` of all possible combinations.
#' @export
#'
#' @examples
#' \dontrun{
#' listcomb(x = 1:3, y = letters[1:2])
#' }
listcomb <- function(...) {
  as.list(expand.grid(..., stringsAsFactors = FALSE))
}

#' Get Combinations
#'
#' @param x a vector with the combination elements.
#' @param prop numeric. the proportion of elements to be drawn.
#' @param maxsam numeric. maximum sample of combinations
#' @param seed numeric. `set.seed()` argument.
#'
#' @return matrix. Each column of the matrix is a different combination.
#' @export
#'
#' @examples
#' \dontrun{
#' combinations(1:10, 0.25, maxsam = 7, seed = 2938)
#' }
combinations <- function(x, prop, maxsam = Inf, seed = 1) {

  x <- unique(x)
  mat_comb <- combn(x, round(prop * length(x)))
  n <- ncol(mat_comb)
  if (maxsam < n) {
    set.seed(seed)
    mat_comb <- mat_comb[ , sample(1:n, size = maxsam)]
  }
  mat_comb
}
