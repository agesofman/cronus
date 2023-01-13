#-------------------------------------------------------------------------------
# Variables
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Rename crops
#'
#' @description
#' Change specific crop names to avoid duplicates.
#'
#' @param crop character. A vector of crop names.
#'
#' @return A character vector of the same dimensions as `crops`.
#'
#' @importFrom stringr str_to_title
#' @export
#'
#' @examples
#' \dontrun{
#' rename_crops(c("Corn", "Hay", "Wheat", "Beans"))
#' }
rename_crops <- function(crop) {
  crop <- stringr::str_to_title(crop)
  crop[crop == "Beans"] <- "Dry Beans"
  crop[crop == "Wheat"] <- "Winter Wheat"
  crop[crop == "Hay"] <- "Alfalfa"
  crop
}

#' @title Truncate value
#'
#' @description
#' Truncate the values of a vector between a lower and an upper limit.
#'
#' @param x numeric. A vector of values to be truncated.
#' @param a numeric. The lower truncation limit.
#' @param b numeric. The upper truncation limit.
#'
#' @return A vector with the same dimensions as `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1:10
#' trunc_minmax(x, 2, 7)
#' }
trunc_minmax <- function(x, a, b) {
  pmax(pmin(x, b), a)
}

#' @title Triangular distribution density
#'
#' @description
#' Calculate the triangular density function value.
#'
#' @param x numeric. A vector of values.
#' @param min numeric. The support minimum.
#' @param max numeric. The support maximum.
#' @param mode numeric. The mode (argmax) of the density function.
#'
#' @return A vector of the same length as `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(0.2, 0.5, -2, 1.5, NA)
#' dtri(x, min = 0, max = 1, mode = 0.5)
#' }
dtri <- function (x, min = 0, max = 1, mode = 0.5) {

  arg_mat <- cbind(x = as.vector(x), min = as.vector(min),
                           max = as.vector(max), mode = as.vector(mode))

  index_na <- is.na(x)
  y <- x
  y[index_na] <- NA
  y_no_na <- y[!index_na]

  for (i in c("x", "min", "max", "mode")) {
    assign(i, arg_mat[!index_na, i])
  }

  mmm <- max - min
  y_no_na <- 2 * ifelse(x <= mode,
                        (x - min)/(mmm * (mode - min)),
                        (max - x)/(mmm * (max - mode)))
  y_no_na[y_no_na < 0] <- 0
  y[!index_na] <- y_no_na
  y

}

#' @title Compute Growing Degree Days (GDD)
#'
#' @description
#' Compute a common expression of thermal time, the Growing Degree Days (GDD).
#'
#' @param x numeric. Minimum temperature.
#' @param y numeric. Maximum temperature.
#' @param a character. Crop name.
#' @param tb_ct matrix. The metadata storing the crop cardinal temperatures.
#'
#' @return A vector with the same dimensions as `x` and `y`.
#' @export
gdd <- function(x, y, a, tb_ct) {

  a <- as.character(a)
  Tb <- tb_ct[a, "Tb"]
  To <- tb_ct[a, "To"]
  Tc <- tb_ct[a, "Tc"]

  x <- trunc_minmax(x, Tb, Tc)
  y <- trunc_minmax(y, Tb, Tc)

  Tav <- (x + y) / 2

  dtri(Tav, min = Tb, max = Tc, mode = To) * (Tc - Tb) / 2

}

#' @title Compute MODIS Normalized Difference Vegetation Index (NDVI)
#'
#' @description
#' Compute MODIS Normalized Difference Vegetation Index (NDVI)
#'
#' @param x numeric. Near-Infrared reflectance.
#' @param y numeric. Red reflectance.
#'
#' @return A vector with the same dimensions as `x` and `y`.
#' @export
ndvi <- function(x, y) {
  x[x < 0] <- NA
  x[x > 1e8] <- NA
  y[y < 0] <- NA
  y[y > 1e8] <- NA
  (y - x) / (y + x)
}

#' @title Compute MODIS cloudmask
#'
#' @description

#' Compute MODIS cloudmask
#'
#' @param x numeric. MODIS QA values.
#'
#' @return A vector with the same dimensions as `x`, taking the values `1` and
#' `NA` for clear and cloudy pixels, respectively.
#'
#' @export
cloudmask <- function(x) {
  v <- matrix(as.numeric(matrix(intToBits(x), ncol = 32, byrow = TRUE)[, 1:3]), ncol = 3)
  r <- rowSums(v[, 1:2])
  r[r == 1] <- NA
  r[r != 1] <- 1
  r
}
