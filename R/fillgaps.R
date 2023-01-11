#-------------------------------------------------------------------------------
# Create missing rasters
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Fill gaps
#'
#' @description `r lifecycle::badge("stable")`
#' Find dates for which the raster is missing and fill the gap with an empty
#' raster.
#'
#' @param x S4 object. The product of interest.
#' @param variable character. The variable of interest.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @importFrom terra rast values writeRaster
#' @importFrom progress progress_bar
#' @export
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## MOD09GA
#'
#' # Create objects
#' x <- new("Mod09ga", region = region, date = date)
#'
#' # Create the NDVI rasters that are missing
#' fillgaps(x, "ndvi")
#'
#' # Create the cloud mask rasters that are missing
#' fillgaps(x, "cloudmask")
#' }
setGeneric("fillgaps", signature = c("x"),
           function(x, ...) { standardGeneric("fillgaps") })

#' @rdname fillgaps
setMethod("fillgaps",
          signature  = c(x = "Satellite"),
          definition = function(x, variable) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Get the toi
  toi <- get_toi(date)

  # Get the directories
  path_variable <- create_db(dir, region, product = product, variable = variable)

  # Find the missing rasters
  ras_all <- file.path(path_variable, paste0(toi$name, ".tif"))
  ras_exist <- list.files(path_variable, full.names = TRUE)
  ras_missing <- setdiff(ras_all, ras_exist)

  # Create a template
  template <- terra::rast(ras_exist[1])
  terra::values(template) <- NA

  # Create a progress bar object
  frm <- paste0("Adding rasters [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(ras_missing), clear = FALSE)

  # Save the rasters
  for (name in ras_missing) {
    pb$tick()
    terra::writeRaster(template, filename = name, overwrite = TRUE, names = basename(name))
  }
  message("Added ", length(ras_missing), " missing rasters.")

})
