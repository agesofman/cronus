#-------------------------------------------------------------------------------
# Mask rasters
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Mask rasters
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Mask a raster variable using anothe raster variable as the mask.
#'
#' @param x S4 object. The product of interest.
#' @param variable1 character. The variable to be masked.
#' @param variable2 character. The variable to be used as a mask.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#' @export
#' @importFrom terra rast project writeRaster
#' @importFrom progress progress_bar
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
#' # Apply a cloud mask on NDVI
#' mask(x, "ndvi", "cloudmask")
#' }
setGeneric("mask", signature = c("x"),
           function(x, ...) { standardGeneric("mask") })

#' @rdname mask
setMethod("mask",
          signature  = c(x = "Satellite"),
          definition = function(x, variable1, variable2 = "cloudmask") {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Get the toi
  toi <- get_toi(date)

  # Get the directories
  dir_var1 <- create_db(dir, region, product = product, variable = variable1)
  dir_var2 <- create_db(dir, region, product = product, variable = variable2)
  path_var1 <- file.path(dir_var1, paste0(toi$name, ".tif"))
  path_var2 <- file.path(dir_var2, paste0(toi$name, ".tif"))

  # Create a progress bar object
  frm <- paste0("Masking ", variable1, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(date), clear = FALSE)

  # Mask the rasters
  for (i in 1:length(date)) {
    pb$tick()
    ras_var1 <- terra::rast(path_var1[i])
    ras_var2 <- terra::rast(path_var2[i])
    ras_var2 <- terra::project(ras_var2, ras_var1, method = "near")
    ras_var1 <- ras_var1 * ras_var2
    terra::writeRaster(ras_var1, filename = path_var1[i], overwrite = TRUE, names = toi$name[i])
  }

})
