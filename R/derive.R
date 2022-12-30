#-------------------------------------------------------------------------------
# Derive Data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Derive data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Derive variables based on existing data.
#'
#' @param x S4 object. A product of interest.
#' @param y S4 object. A product of interest.
#' @param variable character. A function to compute the variable of interest.
#' @param varxy character. Variables used to derive \code{variable}.
#' @param ... extra arguments to \code{variable}.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#' @importFrom terra rast lapp
#' @importFrom tidyr pivot_longer
#' @importFrom progress progress_bar
#' @importFrom zip unzip
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#' dir <- getwd()
#'
#' # Derive MODIS Terra NDVI and cloudmask variables
#' x <- new("Mod09ga", region = region, date = date, dir = dir)
#' derive(x, "ndvi")
#' derive(x, "cloudmask")
#' }
setGeneric("derive", signature = c("x"),
           function(x, ...) { standardGeneric("derive") })

#' @rdname derive
setMethod("derive",
          signature  = c(x = "Daymet"),
          definition = function(x, y, variable, varxy, ...) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Get the toi
  toi <- get_toi(date)

  # Get the directories
  dir_var <- create_db(dir, region, product = product, variable = variable)
  files <- get_files(x, y, variable, varxy)

  # Create a progress bar object
  frm <- paste0("Deriving ", variable, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(date), clear = FALSE)

  # Derive files
  for (i in 1:length(date)) {
    pb$tick()
    path_var <- file.path(dir_var, paste0(toi$name[i], ".tif"))
    y <- terra::rast(files[[i]])
    z <- terra::lapp(y, variable, ..., filename = path_var, overwrite = TRUE, wopt = list(names = toi$name[i]))
  }

})

#' @rdname derive
setMethod("derive",
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
  dir_mosaic <- create_db(dir, region, product = product, variable = "mosaic")
  dir_var <- create_db(dir, region, product = product, variable = variable)
  files <- get_files(x, variable)

  # Create a progress bar object
  frm <- paste0("Deriving ", variable, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(date), clear = FALSE)

  # Derive files
  for (i in 1:length(date)) {
    pb$tick()
    path_mosaic <- file.path(dir_mosaic, paste0(toi$name[i], ".zip"))
    if (file.exists(path_mosaic)) {
      path_var <- file.path(dir_var, paste0(toi$name[i],".tif"))
      zip::unzip(path_mosaic, files = files, exdir = tempdir())
      y <- terra::rast(file.path(tempdir(), files))
      z <- terra::lapp(y, variable, filename = path_var, overwrite = TRUE, wopt = list(names = toi$name[i]))
    } else {
      warning("File ", path_mosaic, " does not exists. Skipping file.")
    }
  }
})
