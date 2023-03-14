#-------------------------------------------------------------------------------
# Refactor data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Refactor data
#'
#' @description
#' Refactor variables using metadata.
#'
#' @param x S4 object. A product of interest.
#' @param variable character. The variable of interest.
#' @param mdname character. The metadata file names needed for the recoding.
#' @param newvarname character. The name of the refactored variable.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#' @importFrom terra terraOptions rast classify set.cats coltab writeRaster setGDALconfig
#' @importFrom progress progress_bar
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
#' # Get the current categories data.frame
#' df_cat <- terra::cats(cdl_2020)[[1]]
#'
#' # Define a transformation table
#' tb_rcl <- cbind(a = c(121:124, 141:143), b = c(rep(82, 4), rep(63, 3)))
#'
#' # Write the metadata
#' write(x, name = "my_metadata", df_cat = df_cat, tb_rcl = tb_rcl)
#'
#' # Refactor the raster
#' refactor(x, variable = "cdl_projected", mdname = "my_metadata", newvarname = "cdl_refactored")
#' }
setGeneric("refactor", signature = c("x"),
           function(x, ...) { standardGeneric("refactor") })

#' @rdname refactor
setMethod("refactor",
          signature  = c(x = "Cropmaps"),
          definition = function(x, variable, mdname, newvarname = variable) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Metadata
  md <- read(x, mdname)
  df_cat <- md$df_cat[-1]
  tb_rcl <- md$tb_rcl

  # Get the toi
  toi <- get_toi(date)
  year <- toi$uyear

  # No progress bar
  terra::terraOptions(progress = 0)

  # Get the directories
  dir_var <- create_db(dir, region, product = product, variable = variable)
  dir_varz <- create_db(dir, region, product = product, variable = newvarname)
  path_var <- file.path(dir_var, paste0(year, ".tif"))
  path_varz <- file.path(dir_varz, paste0(year, ".tif"))

  # Create a progress bar object
  frm <- paste0("Recoding ", variable, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(year), clear = FALSE)

  # Allow for auxiliary files and colours
  terra::setGDALconfig("GDAL_PAM_ENABLED", "TRUE")

  # Refactor the rasters
  for (i in seq_along(year)) {
    pb$tick()
    rast_var <- terra::rast(path_var[i])
    rast_var <- terra::classify(rast_var, tb_rcl, others = NA)
    terra::set.cats(rast_var, value = df_cat)
    terra::coltab(rast_var)[[1]] <- df_cat[, c("RED", "GREEN", "BLUE", "OPACITY")] * 255
    terra::writeRaster(rast_var, filename = path_varz[i], overwrite = TRUE, datatype = "INT1U")
  }

})
