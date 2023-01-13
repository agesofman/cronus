#-------------------------------------------------------------------------------
# Load rasters
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Load rasters
#'
#' @description
#'
#' Load rasters stored in the cronus database.
#'
#' @param x S4 object. A product of interest.
#' @param variable character. The variable of interest.
#' @param date character. Dates of interest.
#' @param ... extra arguments.
#'
#' @return SpatRaster.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## Daymet
#'
#' # Create the object
#' x <- new("Daymet", region = region, date = date)
#'
#' # Load the map
#' variable <- "gdd"
#' load_map(x, variable)
#' load_map(x, variable, as.Date("2002-08-01"))
#' }
setGeneric("load_map", signature = c("x"),
           function(x, ...) { standardGeneric("load_map") })

#' @rdname load_map
setMethod("load_map",
          signature  = c(x = "Product"),
          definition = function(x, variable, date = NULL) {

  # Get slots
  region <- x@region
  dir <- x@dir
  product <- get_product(x)
  if (is.null(date)) {
    date <- x@date
  }
  toi <- get_toi(date)

  # Get the directories
  dir_var <- create_db(dir = dir, region = region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(toi$name, ".tif"))

  # Load the data
  terra::rast(path_var)

})

#' @rdname load_map
setMethod("load_map",
          signature  = c(x = "Cropmaps"),
          definition = function(x, variable, date = NULL) {

  # Get slots
  region <- x@region
  dir <- x@dir
  product <- get_product(x)
  if (is.null(date)) {
    date <- x@date
  }
  toi <- get_toi(date)

  # Get the directories
  dir_var <- create_db(dir = dir, region = region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(toi$uyear, ".tif"))

  # Load the data
  terra::rast(path_var)

})
