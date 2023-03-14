#-------------------------------------------------------------------------------
# Congregate raster data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Congregate raster data
#'
#' @description
#' Congregate raster data into a data frame.
#'
#' @param x S4 object. A product of interest.
#' @param variable character. A function to compute the variable of interest.
#' @param ... extra arguments.
#'
#' @return data.frame. A data frame with the congregated data.
#'
#' @export
#' @importFrom terra rast freq
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## Cropmaps CDL
#'
#' # Create the object
#' x <- new("Cropmaps", region = region, date = date)
#'
#' # congregate
#' a <- congregate(x, "cdl_recoded")
#' }
setGeneric("congregate", signature = c("x"),
           function(x, ...) { standardGeneric("congregate") })

#' @rdname congregate
setMethod("congregate",
          signature  = c(x = "Cropmaps"),
          definition = function(x, variable) {

  # Bind global variables
  layer <- count <- NULL

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Create an rtoi
  toi <- get_toi(date)
  year <- toi$uyear

  # Get the directories
  dir_var <- create_db(dir, region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(year, ".tif"))
  ras_var <- terra::rast(path_var)

  # Compute the frequencies
  x <- terra::freq(ras_var, digits = 0, bylayer = TRUE)
  x <- tidyr::pivot_wider(x, names_from = layer, values_from = count)
  colnames(x) <- c("value", year)
  x[ , -1][is.na(x[ , -1])] <- 0

  # Save the data
  write.csv(x, file.path(dir_var, "summary.csv"))
  invisible(x)

})
