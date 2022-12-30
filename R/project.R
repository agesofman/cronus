#-------------------------------------------------------------------------------
# Project rasters
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Project rasters
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Project a raster variable to match another raster variable.
#'
#' @param x S4 object. A product of interest.
#' @param y S4 object. A product of interest.
#' @param variablex character. The variable of product \code{x}.
#' @param variabley character. The variable of product \code{y}.
#' @param varname character. The name of the projected variable.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#' @importFrom terra terraOptions rast project
#' @importFrom progress progress_bar
#' @importFrom rgdal setCPLConfigOption
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#' dir <- getwd()
#'
#' # Project the CDL to match the Daymet tmin variable
#' x <- new("Cropmaps", region = region, date = date, dir = dir)
#' y <- new("Daymet", region = region, date = date, dir = dir)
#' project(x, y, "cdl", "tmin", "cdl_default")
#' }
setGeneric("project", signature = c("x", "y"),
           function(x, y, ...) { standardGeneric("project") })

#' @rdname project
setMethod("project",
          signature  = c(x = "Cropmaps", y = "Daymet"),
          definition = function(x, y, variablex, variabley, varname = variablex) {

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

  # Get the directories
  dir_varx <- create_db(dir, region, product = productx, variable = variablex)
  dir_vary <- create_db(dir, region, product = producty, variable = variabley)
  dir_varz <- create_db(dir, region, product = productx, variable = varname)
  path_varx <- file.path(dir_varx, paste0(year, ".tif"))
  path_vary <- file.path(dir_vary, paste0(toi$name[1], ".tif"))
  path_varz <- file.path(dir_varz, paste0(year, ".tif"))

  # Create a progress bar object
  frm <- paste0("Projecting ", variablex, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(year), clear = FALSE)

  # Allow for auxiliary files and colours
  rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "TRUE")

  # Raster template
  rast_vary <- terra::rast(path_vary)

  # Project the rasters
  for (i in 1:length(year)) {
    pb$tick()
    rast_varx <- terra::rast(path_varx[i])
    rast_varx <- terra::project(rast_varx, rast_vary, method = "near", filename = path_varz[i], overwrite = TRUE, wopt = list(names = year[i]))
  }

})
