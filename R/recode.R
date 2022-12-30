#-------------------------------------------------------------------------------
# Recode data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Recode data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Recode variables using metadata.
#'
#' @param x S4 object. A product of interest.
#' @param variable character. A function to compute the variable of interest.
#' @param mdname character. The metadata file names needed for the recoding.
#' @param varname character. The name of the recoded variable.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#' @importFrom terra terraOptions rast classify set.cats coltab writeRaster
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
#' x <- new("Cropmaps", region = region, date = date, dir = dir)
#' recode(x, "cdl_default", mdname = "default")
#' }
setGeneric("recode", signature = c("x"),
           function(x, ...) { standardGeneric("recode") })

#' @rdname recode
setMethod("recode",
          signature  = c(x = "Cropmaps"),
          definition = function(x, variable, mdname, varname = variable) {

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
  dir_varz <- create_db(dir, region, product = product, variable = varname)
  path_var <- file.path(dir_var, paste0(year, ".tif"))
  path_varz <- file.path(dir_varz, paste0(year, ".tif"))

  # Create a progress bar object
  frm <- paste0("Recoding ", variable, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(year), clear = FALSE)

  # Allow for auxiliary files and colours
  rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "TRUE")

  # Recode the rasters
  for (i in 1:length(year)) {
    pb$tick()
    rast_var <- terra::rast(path_var[i])
    rast_var <- terra::classify(rast_var, tb_rcl, others = NA)
    terra::set.cats(rast_var, value = df_cat)
    terra::coltab(rast_var)[[1]] = df_cat[, c("red", "green", "blue", "alpha")] * 255
    terra::writeRaster(rast_var, filename = path_varz[i], overwrite = TRUE)
  }

})
