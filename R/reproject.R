#-------------------------------------------------------------------------------
# Reproject rasters
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Reproject rasters
#'
#' @description
#' Reproject a raster variable to match another raster variable.
#'
#' @param x S4 object. A product of interest.
#' @param y S4 object. A product of interest.
#' @param variablex character. The variable of product `x`.
#' @param variabley character. The variable of product `y`.
#' @param newvarname character. The name of the reprojected variable.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#' @importFrom terra terraOptions rast project setGDALconfig
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## Cropmaps CDL and Daymet
#'
#' # Create objects
#' x <- new("Cropmaps", region = region, date = date)
#' y <- new("Daymet", region = region, date = date)
#'
#' # Reproject
#' reproject(x, y, variablex = "cdl", variabley = "tmin", "cdl_reprojected")
#' }
setGeneric("reproject", signature = c("x", "y"),
           function(x, y, ...) { standardGeneric("reproject") })

#' @rdname reproject
setMethod("reproject",
          signature  = c(x = "Cropmaps", y = "Daymet"),
          definition = function(x, y, variablex, variabley, newvarname = variablex) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  productx <- get_product(x)
  producty <- get_product(y)

  # Get the toi
  toi <- get_toi(date)
  year <- toi$uyear

  # No progress bar
  terra::terraOptions(progress = 0)

  # Allow for auxiliary files and colors
  terra::setGDALconfig("GDAL_PAM_ENABLED", "TRUE")

  # Get the directories
  dir_varx <- create_db(dir, region, product = productx, variable = variablex)
  dir_vary <- create_db(dir, region, product = producty, variable = variabley)
  dir_varz <- create_db(dir, region, product = productx, variable = newvarname)
  path_varx <- file.path(dir_varx, paste0(year, ".tif"))
  path_vary <- file.path(dir_vary, paste0(toi$name[1], ".tif"))
  path_varz <- file.path(dir_varz, paste0(year, ".tif"))

  # Create a progress bar object
  frm <- paste0("Reprojecting ", variablex, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(year), clear = FALSE)

  # Raster template
  rast_vary <- terra::rast(path_vary)

  # Reproject the rasters
  for (i in seq_along(year)) {
    pb$tick()
    rast_varx <- terra::rast(path_varx[i])
    terra::project(rast_varx, rast_vary, method = "near", filename = path_varz[i], overwrite = TRUE, wopt = list(datatype = "INT1U", names = year[i]))
  }

})
